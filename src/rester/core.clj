(ns rester.core
  (:gen-class)
  (:require [cheshire.core :as json]
            [cheshire.factory :as factory]
            [clj-http.client :as client]
            [clj-http.conn-mgr :refer [make-reusable-conn-manager]]
            clojure.core
            [clojure.core.async :as async :refer [<! <!! >! chan go go-loop]]
            [clojure.data.xml :refer [emit-str indent parse-str sexp-as-element]]
            [clojure.java.io :as io :refer [make-parents writer]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [clojure.tools.logging :as log]
            [json-path :refer [at-path]]
            [rester.utils
             :refer
             [deep-merge
              load-config
              load-tests-from
              process-tests
              replace-opts
              str->map]])
  (:import [clojure.data.xml CData Element]
           clojure.lang.ExceptionInfo
           java.io.File
           java.lang.Integer
           java.util.Date))

(defn json->clj [json-str]
  (if-not (str/blank? json-str)
    (binding [factory/*json-factory* (factory/make-json-factory
                                      {:allow-unquoted-field-names true})]
      (json/parse-string json-str true))))

(defn- body-to-string [body]
  (cond (instance? Element body) (emit-str body)
        (coll? body) (json/generate-string body)
        (string? body) body
        (nil? body) ""
        :else "(binary)"))

(defn print-curl-request [req]
  (str "curl -v -X" (-> req :verb name str/upper-case) " " (:url req)
       (if-not (empty? (:params req))
         (str (if-not (.contains (:url req) "?") "?") (->> req :params (map #(str/join "=" %)) (#(str/join "&" %))))) " "
       (->> req :headers (map #(str "-H'" (first %) ":" (second %) "' ")) str/join)
       (if-not (empty? (:body req)) (str " -d '" (body-to-string (:body req)) "'"))))

(defn print-http-message [req res]
  (let [format-headers (fn [headers]
                         (->> headers (map #(str/join ":" %)) (str/join "\n")))
        req-str (if (System/getProperty "nocurl")
                  (format "\n%s %s\n%s\nparams:\n%s\npayload:\n%s\n" (:verb req) (:url req)
                          (format-headers (:headers req))
                          (format-headers (:params req)) (body-to-string (:body req)))
                  (print-curl-request req))]

    (format "\n%s\n--------------\nResponse:\nstatus:%d\n%s\nbody:\n%s"
            req-str (:status res) (format-headers (:headers res)) (body-to-string (:body res)))))

(defn diff* [a b]
  (let [ldiff (cond
                (or (and (string? a) (str/blank? a)) (= a b)) nil
                (and (instance? CData a)
                     (instance? CData b)) (diff* (:content a) (:content b))
                (and (instance? Element a)
                     (instance? Element b)) (or (if (not= (:tag a) (:tag b)) a)
                                                (diff* (:attrs a) (:attrs b))
                                                (diff* (:content a) (:content b)))
                (map? a) (if (map? b)
                           (let [d (filter #(diff* (second %) (get b (first %))) a)]
                             (if (empty? d) nil (into {} d)))
                           a)
                (coll? a) (if (coll? b)
                            (let [e (filter (fn [x] (every? #(diff* x %) b)) a)]
                              (if (empty? e) nil (vec e)))
                            a)
                (string? a) (cond
                              (or (and (string? b)
                                       (= (str/trim a) (str/trim b)))
                                  (and (= \# (first a))
                                       (re-find (re-pattern (subs a 1)) (str b)))) nil
                              (re-find #"\s*<[^>]+>" a) (try
                                                          (diff* (parse-str a) (parse-str b))
                                                          (catch Exception e a))
                              :else a)
                (and (nil? a) (some? b)) (str b " not null")
                :else a)]
    (log/debug "diff..." a b " is " ldiff)
    ldiff))

(defn coerce-payload [payload content-type]
  (if (string? payload)
    (try
      (condp re-find (or content-type "json")
        #"xml" (parse-str payload)
        #"text" payload
        #"json" (json->clj payload)
        payload)
      (catch Exception e
        (let [body-str (body-to-string payload)]
          (log/error e "failed coercing payload" body-str)
          body-str)))
    payload))

(def conn-mgr (delay (make-reusable-conn-manager {:insecure? true})))

(defn mk-request [{:keys[id name url verb headers params body] :as  req}]
  (log/infof "executing:%s: %s" name verb url)
  (-> (try
        (client/request {:url url
                         :method verb
                         ;; :content-type :json
                         :headers headers
                         :query-params params
                         :body (if (string? body) body
                                   (and (seq body) (json/generate-string body)))
                         :insecure? true
                         :connection-manager @conn-mgr})
        (catch ExceptionInfo e
          (.getData e)))
      (#(update % :body coerce-payload (get-in % [:headers "Content-Type"])))))

(defn extract-data [{:keys [status body headers] :as resp} extractions]
  (into {} (for [[name path] extractions]
             (try
               (let [result (if (.startsWith path "$")
                              (at-path path body)
                              (at-path (str "$." path) resp))]
                 (log/info "extracted:" name "=" result)
                 [name result])
               (catch Exception e
                 (log/error e "failed extracting " path))))))

(defn verify-response [{:keys [status body headers] :as resp}
                       {{exp-status :status exp-headers :headers exp-body :body} :expect}]
  (or
   (and (not= status exp-status)
        (format "expected status %d, but received %d" exp-status status))
   (some (fn [[header value]]
           (if (diff* (str/trim value) (headers header))
             (str "header " header " was " (headers header) " expected " value))) exp-headers)
   (when-let [ldiff (if exp-body (diff* exp-body body))]
     (log/error "failed matching body. expected:" exp-body " got:" (body-to-string body))
     (->> ldiff (#(if (instance? Element %)
                    (emit-str %)
                    (json/generate-string %)))
          (str "expected body missing:" )))))

(defn replace-values [m opts]
  (reduce-kv #(assoc %1 %2 (replace-opts %3 opts)) m m))

(defn prepare-test [{:keys[expect options] :as test} opts]
  (try
    (let [exp-headers (:headers expect) ]
      (-> test (update :url replace-opts opts)
          (update :headers replace-values opts)
          (update :params replace-values opts)
          (update :body
                  #(-> % (replace-opts opts)
                       ((if (:dont_parse_payload options) identity json->clj))))
          (update-in [:expect :body]
                     #(some-> % not-empty
                              (replace-opts opts)
                              (coerce-payload (get exp-headers "Content-Type"))))))
    (catch Exception e
      (log/error e "error preparing test -> " (:name test))
      (assoc test :error (str "error preparing test due to " e)))))

(defn exec-test-case [test opts]
  (let [test (if (:error test) test (prepare-test test @opts))]
    (if (:error test) test
        (try
          (let [resp (mk-request test)
                delta (verify-response resp test)
                test (assoc test :time (/ (:request-time resp) 1000.0) :resp resp)]
            (if delta
              (do
                (log/warnf "Test failure: [%s/%s], reason :%s" (:suite test) (:name test) delta)
                (assoc test :failure delta))
              (do
                (swap! opts merge (extract-data resp (-> test :options :extractors)))
                (assoc test :success true))))
          (catch Exception e
            (log/errorf e "failed executing test: %s" (:name test))
            (assoc test :error (str (or (.getMessage e) e))))))))

(defn start-executors
  "creates n threads that start waiting for tests in the async channel"
  [test-ch n]
  (let [result-ch (chan (* n 2))]
    (dotimes [i n]
      (go-loop [[t opts] (<! test-ch)]
        (log/infof "executing test:%s/%s" (:suite t) (:name t))
        (when-let [pre-tests (get-in t [:options :before])]
          (doseq [bt pre-tests]
            (try (exec-test-case bt opts)
                 (catch Exception e
                   (log/errorf e "failed executing before test %s..." (:name bt))))))
        (let [r (exec-test-case t opts)]
          (when-let [post-tests (get-in t [:options :after])]
            (doseq [at post-tests]
              (try (exec-test-case at opts)
                   (catch Exception e
                     (log/errorf e "failed executing after test %s..." (:name at))))))
          (>! result-ch r))
        (recur (<! test-ch))))
    result-ch))

(defn children-of [adjs x]
  (concat (adjs x) (distinct (mapcat (partial children-of adjs) (adjs x)))))

(defn mk-adjacents [tests k]
  (->> tests
       (mapcat #(for [d (% k)] [d (:id %)]))
       (reduce #(update %1 (first %2) conj (second %2)) {})))

(defn mk-ordered-iter
  "creates a dependency aware iterator "
  [tests]
  (let [adjs (mk-adjacents tests :deps)
        var-adjs (mk-adjacents tests :var-deps)
        nodes (atom
               (into {} (for [t tests]
                          [(:id t) {:test t :in-degree (count (:deps t))}])))]
    (fn
      ([]
       (let [runnables (->> @nodes (filter #(zero? (:in-degree (second %))))
                            (map (comp :test second)))]
         (reset! nodes (apply dissoc @nodes (map :id runnables)))
         [runnables nil]))
      ([t]
       (let [skipped (when-not (:success t) (children-of var-adjs (:id t)))
             new-nodes (apply dissoc @nodes skipped)
             new-nodes (->> (conj skipped (:id t))
                             (mapcat adjs)
                             (reduce #(if (%1 %2) (update-in %1 [%2 :in-degree] dec) %1) new-nodes))
             runnables (->> new-nodes
                            (filter (comp zero? :in-degree second))
                            (map (comp :test second)))
             skipped (map (comp #(assoc % :skipped (format "dependant test \"%s\" failed" (:name t)))
                                :test @nodes) skipped)
             new-nodes (apply dissoc new-nodes (map :id runnables))]
         (reset! nodes new-nodes)
         [runnables skipped])))))

(defn exec-in-order [tests opts]
  (when-not (empty? tests)
    (let [concurrency (or (:concurrency opts) 4)
          exec-ch (chan (* concurrency 2))
          done-ch (start-executors exec-ch concurrency)
          next-tests-fn (mk-ordered-iter tests)
          [runnables _] (next-tests-fn)
          bindings (atom (:bindings opts))
          num-tests (count tests)]
      (when (empty? runnables)
        (throw (ex-info "No tests ready for execution. Review dependencies" {})))
      (go (doseq [t runnables] (>! exec-ch [t bindings])))
      (<!! (go-loop [r (<! done-ch)
                     pending (dec (count runnables))
                     completed 1
                     results []]
             (let [[runnables skipped] (next-tests-fn r)
                   completed (+ completed (count skipped))
                   pending (+ pending (count runnables))
                   results (apply conj results r skipped)]
               (log/infof "Test %d/%d executed(%d) %s" completed num-tests (:id r)
                          (str (when (not-empty skipped) (str ", skipping:" (count skipped)))))
               (if (empty? runnables)
                 (when (and (zero? pending) (< completed num-tests))
                   (throw (ex-info "failed to complete execution!"
                                   {:pending (- num-tests completed)})))
                 (go (doseq [t runnables] (>! exec-ch [t bindings]))))
               (if (zero? pending)
                 results
                 (recur (<! done-ch) (dec pending) (inc completed) results))))))))

(defn exec-tests [tests opts]
  (let [test-groups (process-tests tests opts)
        executed (exec-in-order (:runnable test-groups) opts)]
    (apply concat executed (-> test-groups (dissoc :runnable) vals))))

(defn suites [tests]
  (reduce (fn [suites test]
            (if (str/blank? (:suite test))
              (update suites (dec (count suites)) update :tests conj test)
              (conj suites {:name (:suite test) :tests [test]})))  [] tests))

(defn summarise-results [tests]
  (let [groups #{:error :failure :success :skipped :ignored :total}
        totals (zipmap groups (repeat 0))
        summarizer-fn (fn [totals t]
                        (let [k (some #(when (t %) %) groups)]
                          (-> totals
                              (update k inc)
                              (update :total inc))))
        suites (->> tests
                    (group-by :suite)
                    (sort-by (comp :id first second))
                    (map #(update % 1 (fn[ts] {:tests (sort-by :id ts)
                                               :summary (reduce summarizer-fn totals ts)}))))]
    {:suites suites
     :summary (->> suites (map (comp :summary second)) (apply merge-with +))}))

(defn print-test-results [{:keys[suites summary]}]
  (flush)
  (println "=============================================================")
  (println (apply format "Test cases: %d, failures:%d, errors:%d, skipped:%d, ignored:%d"
                  (map summary [:total :failure :error :skipped :ignored])))
  (doseq [[suite {tests :tests}] suites]
    (println suite)
    (doseq [test tests
            :let [result (some #(if-let [v (% test)] (str % " " v))
                               [:error :failure :skipped :ignored])]]
      (print (cond
               (:ignored test) "\u001B[93m [_] "
               (:success test) "\u001B[32m [\u2713] "
               (:skipped test) "\u001B[33m [_] "
               :true "\u001B[31m [\u2717] "))
      (println (:name test) "\t" (or result :success) "\u001B[0m"))))

(defn junit-report [opts src-file {suites :suites}]
  (let [dest-file (opts :report
                        (str "target/" (str/replace src-file #"\.[^.]+" "-results.xml")))]
    (make-parents dest-file)
    (with-open [out (writer dest-file)]
      (indent (sexp-as-element
               [:testsuites
                (for [[name {tests :tests {:keys[ignored total failure error skipped]} :summary}] suites]
                  [:testsuite {:name name :errors error :tests total :failures failure}
                   (for [test tests
                         :let [req-log (delay [:-cdata (print-http-message test (:resp test))])]]
                     [:testcase {:name (:name test) :classname src-file :time (:time test)}
                      (condp test nil
                        :error :>> #(vector :error {:message %} @req-log)
                        :failure :>> #(vector :failure {:message %} @req-log)
                        :skipped [:skipped]
                        :ignored [:skipped]
                        :success (when (:verbose opts) @req-log)
                        nil)])])])
              out))))

(defn jsonpath->js [path] path)

(defn tests->postman [name tests opts]
  (let [all-placeholders (into #{} (reduce #(concat %1 (:placeholders %2)) [] tests))
        all-extractions (into #{} (reduce #(concat %1 (:extractors %2)) [] tests))
        all-suites (suites tests)]
    {:info {:name name :schema "https://schema.getpostman.com/json/collection/v2.0.0/collection.json"}
     :variables (for [v (set/difference all-placeholders all-extractions)] {:id v :name v})
     :item (for [{:keys [name tests]} suites]
             {:name name
              :item (for [t tests]
                      {:name (:test t)
                       :request {:url (:url t)
                                 :method (:verb t)
                                 :header (for [[k v] (str->map (:headers t) #":" false)]
                                           {:key k :value v})
                                 :body (:body t)}
                       :event (conj (for [[name path] []] ;; (:extractions t)
                                      {:list "test"
                                       :script (format "var resp=JSON.parse(responseBody);
postman.setEnvironmentVariable(\"%s\", %s);
" name (jsonpath->js path))})
                                    {:listen "test"
                                     :script (format "tests[\"Status code is %1$s\"] = responseCode.code === %1$s;" (:exp-status t))})})})}))

(defn- to-opts [args]
  (->> args
       (partition 2)
       (reduce (fn [result [k v]]
                 (let [k (if-not (.startsWith k ":")
                           (do (log/warn "argument key expected to start with :" k) k)
                           (subs k 1))]
                   (assoc result k v))) {})
       ((fn[m] (update m "commonHeaders" str->map #"=")))
       ((fn[m] (log/info "common headers:" (m "commonHeaders")) m))))

(defn run-tests [file opts]
  (let [[tests-file work-sheet] (str/split file #":" 2)]
    (-> tests-file
        (load-tests-from work-sheet)
        (exec-tests opts)
        (summarise-results))))

(def cli-options
  [["-c" "--config CONFIG" "configuration file"
    :validate [#(.exists (File. %)) "config file not found"]
    :assoc-fn #(assoc %1 %2 (load-config %3))]
   ["-p" "--profiles PROFILES" "comma separated list of profiles"
    :default ["main"]
    :parse-fn #(some-> % not-empty (str/split #","))]
   ["-t" "--concurrency CONCURRENCY" "concurrency level (max number of concurrent test execution)"
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 1000) "Must be a number between 1 and 1000"] ]
   ["-b" "--binding BINDING" "placeholder value as \"SOME_VAR=SOME VALUE\"" :id :bindings :default {}
    :assoc-fn #(update %1 %2 conj (str/split %3 #"=" 2))]
   ["-H" "--header HEADER" "common headers as \"SOME_HEADER=SOME VALUE\"" :id :headers :default {}
    :assoc-fn #(update %1 %2 conj (str/split %3 #"=" 2))]
   ["-s" "--skip SKIP" "skip executing tests with this skip flag"]
   ["-r" "--report REPORT" "report file (defaults to target/<test-file-name>-results.xml)"]
   ["-v" "--verbose" "output HTTP logs"]
   ["-h" "--help"]])

(defn usage [opts]
  (->> ["Usage:"
        "rester [options] <rester-test-cases>.[csv|xls:sheetName|yaml]"
        ""
        "Options:"
        (:summary opts)]
       (str/join \newline )))

(defn -main
  "Executes HTTP(S) requests specified in the argument provided spreadsheet."
  [& args]
  (let [opts (cli/parse-opts args cli-options)]
    (when (-> opts :options :help)
      (println (usage opts))
      (System/exit 0))
    (when (:errors opts)
      (println (:errors opts))
      (println (usage opts))
      (System/exit 1))
    (when (empty? (:arguments opts))
      (println "Missing test case file")
      (println (usage opts))
      (System/exit 1))
    (when (-> opts :options :verbose)
      (println "Parsed Options:")
      (println (pr-str (:options opts))))

    (doseq [p (-> opts :options :profiles)
            f (:arguments opts)
            :let [options (deep-merge (get-in opts [:options :config (keyword p)])
                                      (-> opts :options (dissoc :config)))
                  start (System/currentTimeMillis)
                  _ (log/infof "Executing %s with profile %s : %s" f p (Date.))
                  results (run-tests f options)]]
      (log/infof "Completed in %f secs" (/ (- (System/currentTimeMillis) start) 1000.0))
      ((juxt #(junit-report (:options opts) f %) print-test-results) results))))
