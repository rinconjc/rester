(ns rester.core
  (:gen-class)
  (:require [cheshire
             [core :as json]
             [factory :as factory]]
            [clj-http.client :as client]
            [clojure
             [core :refer [set-agent-send-executor!]]
             [set :refer [union]]
             [string :as str]]
            [clojure.data
             [csv :as csv]
             [xml :refer [emit-str indent parse-str sexp-as-element]]]
            [clojure.java.io :as io :refer [make-parents writer]]
            [clojure.tools.logging :as log]
            [json-path :refer [at-path]])
  (:import [clojure.data.xml CData Element]
           clojure.lang.ExceptionInfo
           java.lang.Integer
           java.util.concurrent.Executors))

(def ^:const fields [:suite :test :url :verb :headers :payload :params :exp-status :exp-body
                     :exp-headers :options :extractions])

(def ^:const placeholder-pattern #"\$(\w+)\$")

(defn placeholders [s]
  (set (map second (re-seq placeholder-pattern s))))

(defn- replace-opts [s opts]
  (if s
    (str/replace s placeholder-pattern
                 #(str (or (opts (second %)) (log/error "missing argument:" (second %)) "")))))

(defn json->clj [json-str]
  (if-not (str/blank? json-str)
    (binding [factory/*json-factory* (factory/make-json-factory
                                      {:allow-unquoted-field-names true})]
      (json/parse-string json-str true))))

(defn str->map
  ([s sep] (str->map s sep {} false))
  ([s sep opts include-empty]
   (if-not (str/blank? s)
     (->> (str/split s #"\s*,\s*")
          (map #(let [[k v] (str/split % sep 2)
                      v (or v (log/warn "missing key or value in " k))]
                  [k (replace-opts v opts)]))
          (filter #(or include-empty (not (str/blank? (second %)))))
          (into {})))))

(defn diff* [a b]
  (let [ldiff (cond
                (or (and (string? a) (str/blank? a)) (= a b)) nil
                (and (instance? CData a) (instance? CData b)) (diff* (:content a) (:content b))
                (and (instance? Element a) (instance? Element b)) (or (if (not= (:tag a) (:tag b)) a)
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
                              (or (and (string? b) (= (str/trim a) (str/trim b)))
                                  (and (= \# (first a)) (re-find (re-pattern (subs a 1)) (str b)))) nil
                              (re-find #"\s*<[^>]+>" a) (try (diff* (parse-str a) (parse-str b))
                                                             (catch Exception e a))
                              :else a)
                :else a)]
    (log/debug "diff..." a b " is " ldiff)
    ldiff))

(defn coerce-payload [payload content-type]
  (try (condp re-find (or content-type "")
         #"xml" (parse-str payload)
         #"text" payload
         (json->clj payload))
       (catch Exception e
         (log/error e "failed coercing payload") payload)))

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
                         :insecure? true})
        (catch ExceptionInfo e
          (.getData e)))
      (#(update % :body coerce-payload (get-in % [:headers "Content-Type"])))))

(defn extract-data [{:keys [status body headers] :as resp} extractions]
  (into {} (for [[name path] extractions]
             (try
               (let [result (at-path path body)]
                 (log/info "extracted:" name "=" result)
                 [name result])
               (catch Exception e
                 (log/error e "failed extracting " path))))))

(defn verify-response [{:keys [status body headers] :as resp}
                       {:keys [exp-status exp-headers exp-body]}]
  (or
   (and (not= status exp-status)
        (str "status " status " not equal to expected " exp-status))
   (some (fn [[header value]]
           (if (not= value (headers header))
             (str "header " header " was " (headers header) " expected " value))) exp-headers)
   (when-let [ldiff (diff* exp-body body)]
     (log/error "failed matching body. expected:" exp-body " got:" body)
     (->> ldiff (#(if (instance? Element %)
                    (emit-str %)
                    (json/generate-string %)))
          (str "expected body missing:" )))))

(defn placeholders-of [{:keys[url headers params payload]}]
  (apply union (map placeholders [url headers params payload])))

(defn cyclic?
  ([deps x] (cyclic? deps x #{}))
  ([deps x visited]
   (or (and (visited x) x)
       (some #(cyclic? deps % (conj visited x)) (deps x)))))

(defn tests-from [file]
  (let [tests (with-open [in-file (io/reader file)]
                (->> (map #(zipmap fields %) (doall (rest (csv/read-csv in-file))))
                     (map-indexed #(-> %2 (assoc :placeholders (placeholders-of %2) :id %1)
                                       (update :extractions str->map #"\s*=\s*")))))
        extractors (for [t tests :when (:extractions t)] [(:id t) (map first (:extractions t))])
        tests (map (fn [{:keys[placeholders] :as test}]
                     (assoc test :deps (for [[i extractions] extractors
                                             :when (some placeholders extractions)] i))) tests)
        tests-with-deps (into {} (filter #(if (:deps %) [(:id %) (:deps %)]) tests))]
    (map #(if (cyclic? tests-with-deps (:id %)) (assoc % :error "circular dependency") %) tests)))

(defn prepare-test [{:keys[exp-headers exp-body options] :as test} opts]
  (try
    (let [options (into #{} (for [opt (str/split options #"\s*,\s*")] (keyword (str/lower-case opt))))
          exp-headers (str->map exp-headers #":")
          exp-body (if-not (str/blank? exp-body)
                     (coerce-payload exp-body (or (get exp-headers "Content-Type") "")))]
      (-> test (update :url replace-opts opts)
          (update :headers str->map #":" opts false)
          (update :params str->map #"\s*=\s*" opts true)
          (update :payload #(if (:dont_parse_payload options) % (json->clj %)))
          (update :exp-status #(Integer/parseInt %))
          (assoc :exp-body exp-body :exp-headers exp-headers)))
    (catch Exception e
      (log/error e "error preparing test " (:test test))
      (assoc test :error (str "error preparing test due to " e)))))

(defn exec-test-case [test opts]
  (let [test (if (:error test) test (prepare-test test @opts))]
    (if (:error test) test
        (let [resp (mk-request test)
              delta (verify-response resp test)
              test (assoc test :time (/ (:request-time resp) 1000.0))]
          (if delta
            (assoc test :failure delta :resp resp)
            (do
              (swap! opts merge (extract-data resp (:extractions test)))
              (assoc test :success true)))))))

(defn exec-tests [tests opts]
  (let [opts (atom opts)
        p (promise)
        count-down (atom (count tests))
        test-agents (vec (map agent tests))
        exec-test (fn[test]
                    (if (:done test) test
                        (let [deps (map #(nth test-agents %) (:deps test))]
                          (if (every? (comp :done deref) deps)
                            (assoc
                             (if (every? (comp :success deref) deps)
                               (try (exec-test-case test opts)
                                    (catch Exception e
                                      (log/error e "failed executing test")
                                      (assoc test :error (str (or (.getMessage e) e)))))
                               (assoc test :skipped "dependents failed"))
                             :done true)
                            test))))]

    (add-watch count-down :key (fn [k r o n]
                                 (when (zero? n)
                                   (log/info "execution complete!")
                                   (deliver p (map deref test-agents)))))
    (doseq [a test-agents i (:deps @a)]
      (add-watch (nth test-agents i) [(:id @a) i] (fn [k r o n] (send-off a exec-test))))
    (doseq [a test-agents]
      (add-watch a [(:id @a)] (fn [k r o n]
                                (if (and (:done n) (not (:done o))) (swap! count-down dec))))
      (send-off a exec-test))
    @p))

(defn summarise-results [tests]
  (let [result-keys {:error 0 :failure 0 :success 0 :skipped 0 :total 0}
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

(defn print-test-results [{:keys[suites total failure error]}]
  (flush)
  (println "=============================================================")
  (println "Test cases:" total ", failed:" failure)
  (doseq [suite suites]
    (println (:name suite))
    (doseq [test (:tests suite)
            :let [result (some #(if-let [v (% test)] (str % " " v)) [:error :failure :skipped])]]
      (print (if (:success test) "\u001B[32m [v] " "\u001B[31m [x] "))
      (println (:test test) "\t" (or result :success) "\u001B[0m"))))

(defn junit-report [dest-file src-file {:keys [suites]}]
  (make-parents dest-file)
  (with-open [out (writer dest-file)]
    (indent (sexp-as-element
             [:testsuites
              (for [{:keys [name tests total failure error skipped]} suites]
                [:testsuite {:name name :errors error :tests total :failures failure}
                 (for [test tests]
                   [:testcase {:name (:test test) :classname src-file :time (:time test)}
                    (condp test nil
                      :error :>> #(vector :error {:message %})
                      :failure :>> #(vector :failure {:message %} (:resp test))
                      :skipped [:skipped]
                      nil)])])])
            out)))

(defn -main
  "Given a CSV file with HTTP request, executes the requests are verifies the expected response"
  [& args]
  (let [pool-size (Integer/parseInt (or (System/getProperty "thread-pool-size") "4"))]
    (log/info "thread pool size:" pool-size)
    (set-agent-send-executor! (Executors/newFixedThreadPool pool-size))
    (set-agent-send-off-executor! (Executors/newFixedThreadPool pool-size)))
  (try
    (let [opts (apply hash-map (map-indexed #(if (even? %1) (subs %2 1) %2) (rest args)))
          _ (println "running with arguments:" opts)
          tests-file (first args)
          xml-report (or (opts "report")
                         (str "target/" (str/replace tests-file #"\.csv" "-results.xml")))
          results (-> tests-file tests-from (exec-tests opts) summarise-results)]
      ((juxt #(junit-report xml-report tests-file %) print-test-results) results)
      (System/exit (- (results :total 0) (results :success 0))))
    (finally
      (shutdown-agents))))
