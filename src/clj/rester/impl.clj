(ns rester.impl
  (:require [cheshire.core :as json]
            [cheshire.factory :as factory]
            [clj-http.client :as client]
            [clj-http.conn-mgr :refer [make-reusable-conn-manager]]
            [clojure.core.async :as async :refer [<! <!! >! chan go go-loop]]
            [clojure.data.xml :refer [emit-str indent parse-str sexp-as-element]]
            [clojure.java.io :as io :refer [make-parents writer]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [json-path :refer [at-path]]
            [yaml.core :as yaml]
            [clojure.spec.alpha :as s]
            [rester.specs :as rs]
            [rester.utils :as utils :refer [str->map map-keys map-values]]
            [ajax.util :as u]
            [rester.utils :as ru])
  (:import [clojure.data.xml CData Element]
           java.text.SimpleDateFormat
           clojure.lang.ExceptionInfo
           java.util.Calendar
           java.lang.Exception))

(def ^:const date-operand-pattern #"\s*(\+|-)\s*(\d+)\s*(\p{Alpha}+)")
(def ^:const date-exp-pattern #"(\p{Alpha}+)((\s*(\+|-)\s*\d+\s*\p{Alpha}+)*)(:(.+))?")
(def ^:const date-fields {"days" Calendar/DATE "day" Calendar/DATE "week" Calendar/WEEK_OF_YEAR
                          "weeks" Calendar/WEEK_OF_YEAR "year" Calendar/YEAR "years" Calendar/YEAR
                          "month" Calendar/MONTH "months" Calendar/MONTH
                          "hour" Calendar/HOUR "hours" Calendar/HOUR
                          "min" Calendar/MINUTE "mins" Calendar/MINUTE
                          "sec" Calendar/SECOND "secs" Calendar/SECOND})

(defn- eval-date-exp [cal [num name]]
  (.add cal (date-fields name Calendar/DATE) num))

(defn- date-from-name [name]
  (let [cal (Calendar/getInstance)]
    (case name
      "now" cal
      "today" cal
      "tomorrow" (do (.add cal Calendar/DATE 1) cal)
      nil)))


(defn parse-date-exp [s]
  (when-let [[_ name operands _ _ _ fmt] (re-matches date-exp-pattern s)]
    (when-let [cal (date-from-name name)]
      (let [operations (for [[_ op n name] (re-seq date-operand-pattern operands)]
                         [(* (if (= op "+") 1 -1) (Integer/parseInt n)) name])]
        (doseq [op operations]
          (eval-date-exp cal op))
        (.format (SimpleDateFormat. (or fmt "yyyy-MM-dd")) (.getTime cal))))))

(defn replace-opts [s opts]
  (cond
    (string? s) (str/replace s utils/placeholder-pattern
                             #(str (or (replace-opts
                                        (opts (second %)
                                              (parse-date-exp (second %))) opts)
                                       (log/error "missing argument:" (second %)) "")))
    (sequential? s) (map #(replace-opts % opts) s)
    :else s))

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
  (log/infof "executing(%s):%s:%s %s" id name verb url)
  (-> (try
        (client/request {:url url
                         :method verb
                         ;; :content-type :json
                         :headers headers
                         :query-params (ru/grouped params first second)
                         :body (if (string? body) body
                                   (and (seq body) (json/generate-string body)))
                         :insecure? true
                         :connection-manager @conn-mgr})
        (catch ExceptionInfo e
          (.getData e))
        (catch Exception e
          (.printStackTrace e)
          ;; (log/error e "request failed" req)
          ))
      (#(update % :body coerce-payload (get-in % [:headers "Content-Type"])))))

(defn piped-json-path [path data]
  (loop [[path & paths] (str/split path #"\|")
         data data]
    (if path
      (recur paths (at-path (if (str/starts-with? path "$") path (str "$" path)) data))
      data)))

(defn extract-data [{:keys [status body headers] :as resp} extractions]
  (into {} (for [[name path] extractions]
             (try
               (let [result (cond
                              (.startsWith path "$") (piped-json-path path body)
                              (.startsWith path "#") (re-find (re-pattern path) body)
                              true (piped-json-path (str "$." path) resp))]
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
     (log/error "failed matching body. expected:" exp-body " got:"
                (-> body body-to-string (str/replace #"^(.{47})(.+)$" "$1...")))
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
                       ((if (or  (:dont_parse_payload options) (false? (:parse-body options))) identity json->clj))))
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
                test (assoc test :time (/ (:request-time resp) 1000.0) :response resp)]
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
        (when-let [pre-tests (get-in t [:options :before])]
          (doseq [bt pre-tests]
            (exec-test-case bt opts)))
        (let [r (exec-test-case t opts)]
          (when-let [post-tests (get-in t [:options :after])]
            (doseq [at post-tests]
              (exec-test-case at opts)))
          (>! result-ch r))
        (recur (<! test-ch))))
    result-ch))

(defn children-of [adjs x]
  (distinct (concat (adjs x) (mapcat (partial children-of adjs) (adjs x)))))

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
         [runnables nil @nodes]))
      ([t]
       (let [skipped (when-not (:success t) (children-of var-adjs (:id t)))
             new-nodes (apply dissoc @nodes skipped)
             new-nodes (->> (conj skipped (:id t))
                            (mapcat adjs)
                            (reduce #(if (%1 %2) (update-in %1 [%2 :in-degree] dec) %1) new-nodes))
             runnables (->> new-nodes
                            (filter (comp zero? :in-degree second))
                            (map (comp :test second)))
             skipped (map (comp #(assoc % :skipped (format "dependent test failed: \"%s\"" (:name t)))
                                :test @nodes) skipped)
             new-nodes (apply dissoc new-nodes (map :id runnables))]
         (reset! nodes new-nodes)
         ;; (log/infof "nodes: %s runnables:%s" (keys new-nodes) (map :id runnables))
         [runnables skipped @nodes])))))

(defn exec-in-order [tests opts]
  (when-not (empty? tests)
    (let [concurrency (or (:concurrency opts) 4)
          exec-ch (chan (* concurrency 2))
          done-ch (start-executors exec-ch concurrency)
          next-tests-fn (mk-ordered-iter tests)
          [runnables] (next-tests-fn)
          bindings (atom (or (:bindings opts) {}))
          num-tests (count tests)]
      (when (empty? runnables)
        (throw (ex-info "No tests ready for execution. Review dependencies" {})))
      (go (doseq [t runnables] (>! exec-ch [t bindings])))
      (<!! (go-loop [r (<! done-ch)
                     pending (dec (count runnables))
                     completed 1
                     results []]
             (let [[runnables skipped remaining] (next-tests-fn r)
                   completed (+ completed (count skipped))
                   pending (+ pending (count runnables))
                   results (apply conj results r skipped)]
               (log/infof "Test %d/%d executed" completed num-tests)
               (if (zero? pending)
                 (if (< completed num-tests)
                   (do (log/fatalf "failed to complete execution....\nlast-test:%s\npending-tests:%s"
                                   (select-keys r [:id :name]) (- num-tests completed))
                       (concat results (map (comp #(assoc % :failure "not executed!") :test) (vals remaining))))
                   results)
                 (do
                   (doseq [t runnables] (go (>! exec-ch [t bindings])))
                   (recur (<! done-ch) (dec pending) (inc completed) results)))))))))

(defn jsonpath->js [path] path)

(defn print-curl-request [req]
  (str "curl -v -X" (-> req :verb name str/upper-case) " '" (:url req)
       (if-not (empty? (:params req))
         (str (if-not (.contains (:url req) "?") "?")
              (client/generate-query-string (:params req)))) "' "
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
                         :let [req-log (delay [:-cdata (print-http-message test (:response test))])]]
                     [:testcase {:name (:name test) :classname src-file :time (:time test)}
                      (condp test nil
                        :error :>> #(vector :error {:message %} @req-log)
                        :failure :>> #(vector :failure {:message %} @req-log)
                        :skipped [:skipped]
                        :ignored [:skipped]
                        :success (when (:verbose opts) @req-log)
                        nil)])])])
              out))))

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
                                 :header (for [[k v] (str->map (:headers t) #":")]
                                           {:key k :value v})
                                 :body (:body t)}
                       :event (conj (for [[name path] []] ;; (:extractions t)
                                      {:list "test"
                                       :script (format "var resp=JSON.parse(responseBody);
postman.setEnvironmentVariable(\"%s\", %s);
" name (jsonpath->js path))})
                                    {:listen "test"
                                     :script (format "tests[\"Status code is %1$s\"] = responseCode.code === %1$s;" (:exp-status t))})})})}))

(defn load-config [file]
  (let [config (->> file yaml/from-file
                    (map-values #(-> % (update :bindings (partial map-keys name))
                                     (update :headers (partial map-keys name)))))]
    (if (s/valid? ::rs/config config)
      config
      (throw (ex-info "Invalid configuration format" :error (s/explain ::rs/config config))))))
