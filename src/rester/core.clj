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
            [clojure.tools.logging :as log]
            [json-path :refer [at-path]]
            [rester.utils :refer [load-tests-from replace-opts str->map process-tests]])
  (:import [clojure.data.xml CData Element]
           clojure.lang.ExceptionInfo
           java.lang.Integer))

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
  (str "curl -v -X" (:verb req) " " (:url req)
       (if-not (empty? (:params req))
         (str (if-not (.contains (:url req) "?") "?") (->> req :params (map #(str/join "=" %)) (#(str/join "&" %))))) " "
       (->> req :headers (map #(str "-H'" (first %) ":" (second %) "' ")) str/join)
       (if-not (empty? (:payload req)) (str " -d '" (body-to-string (:payload req)) "'"))))

(defn print-http-message [req res]
  (let [format-headers (fn [headers]
                         (->> headers (map #(str/join ":" %)) (str/join "\n")))
        req-str (if (System/getProperty "nocurl")
                  (format "\n%s %s\n%s\nparams:\n%s\npayload:\n%s\n" (:verb req) (:url req)
                          (format-headers (:headers req))
                          (format-headers (:params req)) (body-to-string (:payload req)))
                  (print-curl-request req))]

    (format "\n%s\n--------------\nresponse status:%d\n%s\nbody:\n%s"
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
        nil)
      (catch Exception e
        (let [body-str (body-to-string payload)]
          (log/error e "failed coercing payload" body-str)
          body-str)))
    payload))

(def conn-mgr (delay (make-reusable-conn-manager {:insecure? true})))

(defn mk-request [{:keys[test url verb headers params payload] :as  req}]
  (log/info "executing" test ": " verb " " url)
  (-> (try
        (client/request {:url url
                         :method (keyword (str/lower-case verb))
                         ;; :content-type :json
                         :headers headers
                         :query-params params
                         :body (if (string? payload) payload
                                   (and (seq payload) (json/generate-string payload)))
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
    (let [exp-headers (:headers expect)
          exp-body (some-> (:body expect)
                           not-empty
                           (replace-opts opts)
                           (coerce-payload (or (get exp-headers "Content-Type") "")))]
      (-> test (update :url replace-opts opts)
          (update :headers replace-values opts)
          (update :headers merge (opts "commonHeaders"))
          (update :params replace-values opts)
          (update :payload #(-> % (replace-opts opts)
                                ((if (:dont_parse_payload options) identity json->clj))))
          (assoc-in [:expect :body] exp-body)))
    (catch Exception e
      (log/error e "error preparing test -> " (:test test))
      (assoc test :error (str "error preparing test due to " e)))))

(defn exec-test-case [test opts]
  (let [test (if (:error test) test (prepare-test test @opts))]
    (if (:error test) test
        (try
          (let [resp (mk-request test)
                delta (verify-response resp test)
                test (assoc test :time (/ (:request-time resp) 1000.0) :resp resp)]
            (if delta
              (assoc test :failure delta)
              (do
                (swap! opts merge (extract-data resp (:extractions test)))
                (assoc test :success true))))
          (catch Exception e
            (log/error e "failed executing test")
            (assoc test :error (str (or (.getMessage e) e))))))))

(defn start-executors
  "creates n threads that start waiting for tests in the async channel"
  [result-ch n]
  (let [test-ch (chan (* n 2))]
    (dotimes [i n]
      (go-loop [[t opts] (<! test-ch)]
        (when (:before t)
          (doseq [bt (:before t)]
            (try (exec-test-case bt opts)
                 (catch Exception e
                   (log/errorf e "failed executing before test %s..." (:name bt))))))
        (let [r (exec-test-case t opts)]
          (when (:after t)
            (doseq [at (:after t)]
              (try (exec-test-case at opts)
                   (catch Exception e
                     (log/errorf e "failed executing after test %s..." (:name at))))))
          (>! result-ch r))
        (recur (<! test-ch))))
    result-ch))

(defn children-of [adjs x]
  (concat (adjs x) (distinct (mapcat (partial children-of adjs) (adjs x)))))

(defn mk-ordered-iter
  "creates a dependency aware iterator "
  [tests]
  (let [adjs (->> tests
                  (mapcat #(for [d (:deps %)] [d (:id %)]))
                  (reduce #(update %1 (first %2) conj (second %2)) {}))
        nodes (atom
               (into {} (for [t tests]
                          [(:id t) {:test t :in-degree (count (:deps t))}])))]
    (fn
      ([] (filter #(= 0 (:in-degree (second %))) @nodes))
      ([t]
       (let [child-tests (if (:success t) (adjs (:id t)) (children-of adjs (:id t)))
             [next-nodes next-ts]
             (if (:success t)
               (reduce
                (fn [[nodes zero-degs] a]
                  (let [nodes (update nodes a update :in-degree dec)]
                    (if (zero? (:in-degree (nodes a)))
                      [nodes (conj zero-degs (nodes a))]
                      [nodes zero-degs])))
                [@nodes []]
                child-tests)
               [(apply dissoc @nodes child-tests)
                (some->> child-tests (map @nodes)
                         (map #(assoc % :skipped (format "dependant [%s] failed!" (:name t)))))])]
         (reset! nodes next-nodes)
         next-ts)))))

(defn exec-in-order [tests opts]
  (let [concurrency (or (:concurrency opts) 4)
        exec-ch (chan (* concurrency 2))
        done-ch (start-executors exec-ch concurrency)
        next-tests-fn (mk-ordered-iter tests)
        runnables (next-tests-fn)
        opts (atom opts)
        num-tests (count tests)]
    (when (empty? runnables)
      (throw (Exception. "No tests ready for execution. Review dependencies")))
    (go (doseq [t runnables] (>! exec-ch [t opts])))
    (<!! (go-loop [r (<! done-ch)
                   executed 0
                   results []]
           (let [next-tests (not-empty (next-tests-fn r))
                 runnables (when (:success r) next-tests)
                 done-tests (when-not (:success r) next-tests)
                 executed (+ executed (count done-tests) 1)]
             (log/infof "Test %i/%i executed" executed num-tests)
             (when runnables
               (go (doseq [t runnables] (>! exec-ch [t opts]))))
             (if (= executed num-tests)
               results
               (recur (<! done-ch) executed (apply conj results r done-tests))))))))

(defn exec-tests [tests opts]
  (let [test-groups (process-tests tests opts)
        executed (exec-in-order (:runnable test-groups) opts)]
    (apply concat executed (-> test-groups (dissoc :runnable) vals))))

;; (defn exec-tests [tests opts]
;;   (let [skip-tag (opts "skip")
;;         opts (atom opts)
;;         name-to-test (into {} (for [t tests] [(:test t) t]))
;;         test-agents (vec (map agent tests))
;;         exec-test (fn [test]
;;                     (if (:done test) test
;;                         (let [{:keys [before after skip ignore]} (:options test)
;;                               deps (map #(nth test-agents %) (:deps test))
;;                               pending (filter (comp not :done deref) deps)]
;;                           (if (empty? pending)
;;                             (assoc
;;                              (cond
;;                                (and skip-tag (= skip-tag skip)) (assoc test :ignored "skip requested")
;;                                (true? ignore) (assoc test :ignored "ignored")
;;                                (every? (comp :success deref) deps)
;;                                (try
;;                                  (if before (doseq [t (map name-to-test (str/split before #";"))]
;;                                               (if t (exec-test-case t opts))))
;;                                  (exec-test-case test opts)
;;                                  (catch Exception e
;;                                    (log/error "before test failed:" e))
;;                                  (finally
;;                                    (if after (doseq [t (map name-to-test (str/split after #";"))]
;;                                                (if t (exec-test-case t opts))))))
;;                                :else (assoc test :skipped "dependents failed"))
;;                              :done true)
;;                             (do
;;                               (log/infof "%s blocked by %s" (:id test) (str/join "," (map (comp :id deref) pending)))
;;                               test)))))]

;;     (doseq [[priority agents] (sort-by first (group-by (comp :priority deref) test-agents))
;;             :let [pending (atom (into #{} (map (comp :id deref) agents)))
;;                   p (promise)]]
;;       (doseq [a agents dep (:deps @a)]
;;         (add-watch (nth test-agents dep) [(:id @a) dep]
;;                    (fn [k r o n] (send-off a exec-test))))
;;       (doseq [a agents]
;;         (add-watch a [(:id @a)]
;;                    (fn [k r o n] (when (:done n)
;;                                    (if (empty? (swap! pending #(remove #{(:id n)} %1)))
;;                                      (deliver p :done))
;;                                    (log/debug "completed:" (:id n) " pending" @pending))))
;;         (send-off a exec-test))
;;       @p)
;;     (map deref test-agents)))

(defn suites [tests]
  (reduce (fn [suites test]
            (if (str/blank? (:suite test))
              (update suites (dec (count suites)) update :tests conj test)
              (conj suites {:name (:suite test) :tests [test]})))  [] tests))

(defn summarise-results [tests]
  (let [result-keys {:error 0 :failure 0 :success 0 :skipped 0 :ignored 0 :total 0 :invalid 0}
        add-results (fn [r1 r2]
                      (into {} (for [k (keys result-keys)]
                                 [k (+ (r1 k 0) (r2 k 0))])))]
    (->> tests
         (reduce
          (fn [suites test]
            (if (str/blank? (:suite test))
              (update suites (dec (count suites)) update :tests conj test)
              (conj suites {:name (:suite test) :tests [test]}))) [])
         (map (fn [suite]
                (merge suite
                       (reduce (fn[s t]
                                 (-> s (update (or (some #(if (t %) %) (keys s))
                                                   (log/warn "unexpected:" t) :error) inc)
                                     (update :total inc)))
                               result-keys (:tests suite)))))
         (#(assoc (reduce add-results %) :suites %)))))

(defn print-test-results [{:keys[suites total failure error skipped ignored]}]
  (flush)
  (println "=============================================================")
  (println "Test cases:" total ", failed:" failure ", skipped:" skipped ", ignored:" ignored)
  (doseq [suite suites]
    (println (:name suite))
    (doseq [test (:tests suite)
            :let [result (some #(if-let [v (% test)] (str % " " v)) [:error :failure :skipped])]]
      (print (cond
               (:success test) "\u001B[32m [v] "
               (:ignored test) "\u001B[32m [_] "
               :true "\u001B[31m [x] "))
      (println (:test test) "\t" (or result :success) "\u001B[0m"))))

(defn junit-report [opts src-file {:keys [suites]}]
  (let [dest-file (opts "report.file"
                        (str "target/" (str/replace src-file #"\.[^.]+" "-results.xml")))]
    (make-parents dest-file)
    (with-open [out (writer dest-file)]
      (indent (sexp-as-element
               [:testsuites
                (for [{:keys [name tests total failure error skipped]} suites]
                  [:testsuite {:name name :errors error :tests total :failures failure}
                   (for [test tests :let [req-log (delay [:-cdata (print-http-message test (:resp test))])]]
                     [:testcase {:name (:test test) :classname src-file :time (:time test)}
                      (condp test nil
                        :error :>> #(vector :error {:message %} @req-log)
                        :failure :>> #(vector :failure {:message %} @req-log)
                        :skipped [:skipped]
                        :ignored [:skipped]
                        :success (if (opts "report.http-log") @req-log)
                        nil)])])])
              out))))

(defn jsonpath->js [path] path)

(defn tests->postman [name tests opts]
  (let [all-placeholders (into #{} (reduce #(concat %1 (:placeholders %2)) [] tests))
        all-extractions (into #{} (reduce #(concat %1 (:extractions %2)) [] tests))
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
                                 :body (:payload t)}
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

(defn run-tests [args]
  (let [pool-size (Integer/parseInt (or (System/getProperty "thread-pool-size") "4"))
        opts (-> args rest to-opts (assoc :concurrency pool-size))
        tests-file (first args)
        results (-> tests-file
                    (load-tests-from (opts "sheet"))
                    (exec-tests opts)
                    summarise-results)]
    (log/info "thread pool size:" pool-size)
    ((juxt #(junit-report opts tests-file %) print-test-results) results)
    (System/exit (+ (get results :error 0) (get results :failure 0)))))

(defn -main
  "Executes HTTP requests specified in the argument provided spreadsheet."
  [& args]
  (when-not (seq args)
    (println "Usage: \njava -jar rester-0.1.0-beta2.jar <command> <rest-test-cases.csv> [placeholder replacements as :placeholder-name value]
or
lein run -m rester.core <rest-test-cases.csv> [placeholder replacements as :placeholder-name value]")
    (System/exit 1))
  (run-tests args))
