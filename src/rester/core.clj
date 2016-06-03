(ns rester.core
  (:gen-class)
  (:require [cheshire
             [core :as json]
             [factory :as factory]]
            [clj-http.client :as client]
            [clojure.data
             [csv :as csv]
             [xml :refer [emit emit-str parse-str sexp-as-element]]]
            [clojure.java.io :as io :refer [make-parents writer]]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.data.xml :refer [indent]])
  (:import clojure.data.xml.Element
           clojure.lang.ExceptionInfo))

(defn- replace-opts [s opts]
  (str/replace s #"\$(\w+)\$" #(or (opts (second %)) (log/error "missing argument:" (second %)))))

(defn- json->clj [json-str]
  (if-not (str/blank? json-str)
    (binding [factory/*json-factory* (factory/make-json-factory
                                      {:allow-unquoted-field-names true})]
      (json/parse-string json-str))))

(defn- str->map
  ([s sep] (str->map s sep {} false))
  ([s sep opts include-empty]
   (if-not (str/blank? s)
     (->> (str/split s #"\s*,\s*")
          (map #(let [[k v] (str/split % sep 2)] [k (replace-opts v opts)]))
          (filter #(or include-empty (not (str/blank? (second %)))))
          (into {})))))

(defn diff* [a b]
  (let [ldiff (cond
                (or (and (string? a) (str/blank? a)) (= a b)) nil
                (and (instance? Element a) (instance? Element b)) (or (if (not= (:tag a) (:tag b)) a)
                                                                      (diff* (:attrs a) (:attrs b))
                                                                      (diff* (:content a) (:content b)))
                (map? a) (if (map? b)
                           (let [d (filter #(diff* (second %) (b (first %))) a)]
                             (if (empty? d) nil (into {} d)))
                           a)
                (coll? a) (if (coll? b)
                            (let [e (filter (fn [x] (every? #(diff* x %) b)) a)]
                              (if (empty? e) nil (vec e)))
                            a)
                (string? a) (if (and (= \# (first a)) (re-find (re-pattern (subs a 1)) (str b)))
                              nil a)

                :else a)]
    (log/debug "diff..." a b " is " ldiff)
    ldiff))

(defn load-tests-from [file opts]
  (with-open [in-file (io/reader file)]
    (reduce
     (fn [suites [suite test url verb headers payload params exp-status exp-body exp-headers options]]
       (let [options (into #{} (for [opt (str/split options #"\s*,\s*")] (keyword (str/lower-case opt))))
             exp-headers (str->map exp-headers #":")
             exp-body (if-not (str/blank? exp-body)
                        (try (condp re-find (or (get exp-headers "Content-Type") "")
                               #"xml" (parse-str exp-body)
                               (json->clj exp-body))
                             (catch Exception e
                               (log/error "failed parsing exp-body" e) exp-body)))
             test-case {:test test :url (replace-opts url opts) :verb verb
                        :headers (str->map headers #":" opts false)
                        :params (str->map params #"\s*=\s*" opts true)
                        :payload (if (:dont_parse_payload options) payload (json->clj payload))
                        :exp-status (Integer/parseInt exp-status)
                        :exp-body exp-body
                        :exp-headers exp-headers}]
         (if-not (str/blank? suite)
           (conj suites {:suite suite
                         :tests [test-case]})
           (update suites (dec (count suites)) update :tests conj test-case))))
     []
     (rest (csv/read-csv in-file)))))

(defn mk-request [{:keys[test url verb headers params payload]}]
  (log/info "executing" test ": " verb " " url)
  (time (try
          (client/request {:url url
                           :method (keyword (str/lower-case verb))
                           ;; :content-type :json
                           :headers headers
                           :query-params params
                           :body (if (string? payload) payload
                                     (and (seq payload) (json/generate-string payload)))
                           :insecure? true})
          (catch ExceptionInfo e
            (.getData e)))))

(defn verify-response [{:keys [status body headers] :as resp}
                       {:keys [exp-status exp-headers exp-body]}]
  (let [body* (condp re-find (or (:Content-Type headers) "")
                #"json" (json->clj body)
                #"xml" (parse-str body)
                body)
        error (or
               (and (not= status exp-status)
                    (str "status " status " not equal to expected " exp-status))
               (some (fn [[header value]]
                       (if (not= value (headers header))
                         (str "header " header " was " (headers header) " expected " value))) exp-headers)
               (when-let [ldiff (diff* exp-body body*)]
                 (log/error "failed matching body. expected:" exp-body " got:" body*)
                 (->> ldiff (#(if (instance? Element %)
                                (emit-str %)
                                (json/generate-string %)))
                      (str "expected body missing:" ))))]
    (if error
      [error resp])))

(defn test-all [suites]
  (-> (for [{:keys[suite tests]} suites
            :let [results (doall (map #(future [(% :test)
                                                (verify-response (mk-request %) %)]) tests))]]
        {:suite suite
         :results results
         :count (count results)
         :failures (count (filter #(second @%) results))})
      (#(hash-map :suites %
                  :total (reduce + (map :count %))
                  :total-failures (reduce + (map :failures %))))))

(defn print-test-results [{:keys[suites total total-failures]}]
  (flush)
  (println "=============================================================")
  (println "Test cases:" total ", failed:" total-failures)
  (doseq [{:keys [suite results]} suites]
    (println suite)
    (doseq [result results :let [[test result] @result]][]
           (print (if result "\u001B[31m [x] " "\u001B[32m [v] "))
           (println test "\t" (or (first result) :OK) "\u001B[0m"))))

(defn junit-report [dest-file {:keys[suites total total-failures]}]
  (make-parents dest-file)
  (with-open [out (writer dest-file)]
    (indent (sexp-as-element
             [:testsuites
              (for [{:keys [suite results count failures]} suites]
                [:testsuite {:name suite :errors 0 :tests count :failures failures}
                 (for [r results :let [[test result] @r]]
                   [:testcase {:name test :time 0}
                    (if result [:failure {:message (first result)} (second result)])])])])
            out)))

(defn -main
  "Give me a CSV with API rest cases and I will verify them"
  [& args]
  (try
    (let [opts (apply hash-map (map-indexed #(if (even? %1) (subs %2 1) %2) (rest args)))
          _ (println "running with arguments:" opts)
          xml-report (or (opts "report")
                         (str "target/" (str/replace (first args) #"\.csv" "-results.xml")))
          results (test-all (load-tests-from (first args) opts))]
      ((juxt #(junit-report xml-report %) print-test-results) results)
      (System/exit (:total-failures results)))
    (finally
      (shutdown-agents))))
