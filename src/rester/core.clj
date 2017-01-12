(ns rester.core
  (:gen-class)
  (:require [cheshire
             [core :as json]
             [factory :as factory]]
            [clj-http
             [client :as client]
             [conn-mgr :refer [make-reusable-conn-manager]]]
            [clojure
             [core :refer [set-agent-send-executor!]]
             [set :refer [union]]
             [string :as str]]
            [clojure.data
             [csv :as csv]
             [xml :refer [emit-str indent parse-str sexp-as-element]]]
            [clojure.java.io :as io :refer [make-parents writer]]
            [clojure.tools.logging :as log]
            [dk.ative.docjure.spreadsheet
             :refer
             [load-workbook read-cell row-seq select-sheet sheet-seq]]
            [json-path :refer [at-path]])
  (:import [clojure.data.xml CData Element]
           clojure.lang.ExceptionInfo
           java.lang.Integer
           java.text.SimpleDateFormat
           java.util.Calendar
           java.util.concurrent.Executors))

(def ^:const fields [:suite :test :url :verb :headers :payload :params :exp-status :exp-body
                     :exp-headers :options :extractions])

(def ^:const placeholder-pattern #"\$(\p{Alpha}[^\$]*)\$")
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
  (if-let [[_ name operands _ _ _ fmt] (re-matches date-exp-pattern s)]
    (if-let [cal (date-from-name name)]
      (let [operations (for [[_ op n name] (re-seq date-operand-pattern operands)]
                         [(* (if (= op "+") 1 -1) (Integer/parseInt n)) name])]
        (doseq [op operations]
          (eval-date-exp cal op))
        (.format (SimpleDateFormat. (or fmt "yyyy-MM-dd")) (.getTime cal))))))

(defn placeholders [s]
  (set (map second (re-seq placeholder-pattern s))))

(defn- replace-opts [s opts]
  (if s
    (str/replace s placeholder-pattern
                 #(str (or (opts (second %) (parse-date-exp (second %)))
                           (log/error "missing argument:" (second %)) "")))))

(defn json->clj [json-str]
  (if-not (str/blank? json-str)
    (binding [factory/*json-factory* (factory/make-json-factory
                                      {:allow-unquoted-field-names true})]
      (json/parse-string json-str true))))

(defn str->map
  ([s sep] (str->map s sep {} false))
  ([s sep opts include-empty]
   (if-not (str/blank? s)
     (let [[sep1 sep] (if (vector? sep) sep [#"\s*,\s*" sep])]
       (->> (str/split s sep1)
            (map #(let [[k v] (str/split % sep 2)
                        v (or v (log/warn "missing key or value in " k))]
                    [k (replace-opts v opts)]))
            (filter #(or include-empty (not (str/blank? (second %)))))
            (into {}))))))

(defn- body-to-string [body]
  (cond (instance? Element body) (emit-str body)
        (coll? body) (json/generate-string body)
        :else body))

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
                (and (nil? a) (some? b)) (str b " not null")
                :else a)]
    (log/debug "diff..." a b " is " ldiff)
    ldiff))

(defn coerce-payload [payload content-type]
  (try (condp re-find (or content-type "")
         #"xml" (parse-str payload)
         #"text" payload
         (json->clj payload))
       (catch Exception e
         (log/error e "failed coercing payload" payload)
         payload)))

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
                       {:keys [exp-status exp-headers exp-body]}]
  (or
   (and (not= status exp-status)
        (format "expected status %d, but received %d" exp-status status))
   (some (fn [[header value]]
           (if (diff* (str/trim value) (headers header))
             (str "header " header " was " (headers header) " expected " value))) exp-headers)
   (when-let [ldiff (if exp-body (diff* exp-body body))]
     (log/error "failed matching body. expected:" exp-body " got:" body)
     (->> ldiff (#(if (instance? Element %)
                    (emit-str %)
                    (json/generate-string %)))
          (str "expected body missing:" )))))

(defn placeholders-of [{:keys[url headers params payload exp-body]}]
  (apply union (map placeholders [url headers params payload exp-body])))

(defn cyclic?
  ([deps x] (cyclic? deps x #{}))
  ([deps x visited]
   (or (and (visited x) x)
       (some #(cyclic? deps % (conj visited x)) (deps x)))))

(defmulti read-rows (fn [file _]
                      (cond
                        (str/ends-with? file ".csv") :csv
                        (str/ends-with? file ".xlsx") :excel)))

(defmethod read-rows :excel [file sheet]
  (let [wk-book (load-workbook file)
        wk-sheet (cond
                   (string? sheet) (select-sheet sheet wk-book)
                   :else (-> wk-book sheet-seq first))]
    (into [] (->> wk-sheet row-seq
                  (map #(map (fn[i] (if-let [c (.getCell % i)] (str (read-cell c)) ""))
                             (range (count fields))))
                  rest))))

(defmethod read-rows :csv [file _]
  (with-open [in-file (io/reader file)]
    (doall (rest (csv/read-csv in-file)))))

(defn tests-from [file sheet]
  (let [tests (->> (map #(zipmap fields %) (read-rows file sheet))
                   (filter #(not (str/blank? (:test %))))
                   (map-indexed #(-> %2 (assoc :placeholders (placeholders-of %2) :id %1)
                                     (update :extractions str->map #"\s*=\s*"))))
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
                     (coerce-payload (replace-opts exp-body opts) (or (get exp-headers "Content-Type") "")))]
      (-> test (update :url replace-opts opts)
          (update :headers str->map #":" opts false)
          (update :params str->map  [#"&|(\s*,\s*)" #"\s*=\s*"] opts true)
          (update :payload #(-> % (replace-opts opts) ((if (:dont_parse_payload options) identity json->clj))))
          (update :exp-status #(.intValue (Double/parseDouble %)))
          (assoc :exp-body exp-body :exp-headers exp-headers)))
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

(defn exec-tests [tests opts]
  (let [opts (atom opts)
        skip-tag (:skip opts)
        p (promise)
        count-down (atom (count tests))
        test-agents (vec (map agent tests))
        exec-test (fn[test]
                    (if (:done test) test
                        (let [deps (map #(nth test-agents %) (:deps test))]
                          (if (every? (comp :done deref) deps)
                            (assoc
                             (cond
                               (and skip-tag (= skip-tag (-> test :options :skip))) (assoc test :skipped "skip requested")
                               (every? (comp :success deref) deps) (exec-test-case test opts)
                               :else (assoc test :skipped "dependents failed"))
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
                        :success (if (opts "report.http-log") @req-log)
                        nil)])])])
              out))))

(defn -main
  "Executes HTTP requests specified in the argument provided spreadsheet."
  [& args]
  (when-not (seq args)
    (println "Usage: \njava -jar rester-0.1.0-beta2.jar <rest-test-cases.csv> [placeholder replacements as :placeholder-name value]
or
lein run -m rester.core <rest-test-cases.csv> [placeholder replacements as :placeholder-name value]")
    (System/exit 1))

  (let [pool-size (Integer/parseInt (or (System/getProperty "thread-pool-size") "4"))]
    (log/info "thread pool size:" pool-size)
    (set-agent-send-executor! (Executors/newFixedThreadPool pool-size))
    (set-agent-send-off-executor! (Executors/newFixedThreadPool pool-size)))
  (try
    (let [opts (apply hash-map (map-indexed #(if (even? %1) (subs %2 1) %2) (rest args)))
          tests-file (first args)
          results (-> tests-file (tests-from (opts "sheet")) (exec-tests opts) summarise-results)]
      ((juxt #(junit-report opts tests-file %) print-test-results) results)
      (System/exit (- (results :total 0) (results :success 0))))
    (finally
      (shutdown-agents))))
