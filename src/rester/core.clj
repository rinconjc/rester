(ns rester.core
  (:gen-class)
  (:require [cheshire
             [core :as json]
             [factory :as factory]]
            [clj-http.client :as client]
            [clojure
             [data :as data]
             [string :as str]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log])
  (:import clojure.lang.ExceptionInfo))

(defn- replace-opts [s opts]
  (str/replace s #"\$(\w+)\$" #(opts (second %))))

(defn- json->clj [json-str]
  (if-not (str/blank? json-str)
    (binding [factory/*json-factory* (factory/make-json-factory
                                      {:allow-unquoted-field-names true})]
      (json/parse-string json-str))))

(defn- str->map
  ([s sep] (str->map s sep {}))
  ([s sep opts]
   (if-not (str/blank? s)
     (->> (str/split s #"\s*,\s*") (map #(let [[k v] (str/split % sep 2)]
                                           [k (replace-opts v opts)])) (into {})))))

(defn tests-from [file opts]
  (with-open [in-file (io/reader file)]
    (reduce
     (fn[suites [suite test url verb headers payload params exp-status exp-body exp-headers]]
       (let [test-case {:test test :url (replace-opts url opts) :verb verb
                        :headers (str->map headers #":" opts)
                        :params (str->map params #"\s*=\s*" opts)
                        :payload (json->clj payload)
                        :exp-status (Integer/parseInt exp-status)
                        :exp-body (json->clj exp-body)
                        :exp-headers (str->map exp-headers #":")}]
         (if-not (str/blank? suite)
           (conj suites {:suite suite
                         :tests [test-case]})
           (update suites (dec (count suites)) update :tests conj test-case))))
     []
     (rest (csv/read-csv in-file)))))

(defn mk-request [{:keys[test url verb headers params payload]}]
  (log/info "executing" test ": " verb " " url)
  (try
    (client/request {:url url
                     :method (keyword (str/lower-case verb))
                     :headers headers
                     :query-params params
                     :body payload})
    (catch ExceptionInfo e
      (.getData e))))

(defn verify-response [{:keys [status body headers] :as resp}
                       {:keys [exp-status exp-headers exp-body]}]
  (let [body* (case (:Content-Type headers) "application/json" (json->clj body) body)
        error (or
               (and (not= status exp-status)
                    (str "status " status " not equal to expected " exp-status))
               (some (fn [[header value]]
                        (if (not= value (headers header))
                          (str "header " header " was " (headers header) " expected " value))) exp-headers)
               (if-let [diff (and (not-empty exp-body) (first (data/diff exp-body body*)))]
                 (->> diff json/generate-string (str "expected body missing:" ))))]
    (if error [error resp])))

(defn test-all [suites]
  (for [{:keys[suite tests]} suites
        :let [results (doall (map #(future [(% :test)
                                            (verify-response (mk-request %) %)]) tests))]]
    {:suite suite
     :results results
     :total (count results)
     :failures (count (filter #(second @%) results))}))

(defn print-test-results [suites]
  (let [total (reduce + (map :total suites))
        fail-count (reduce + (map :failures suites))]
    (println "=============================================================")
    (println "Test cases:" total ", failed:" fail-count)
    (doseq [{:keys [suite results]} suites]
      (println suite)
      (doseq [result results :let [[test result] @result]]
        (print (if result "\u001B[31m [x] " "\u001B[32m [v] "))
        (println test "\t" (or (first result) :OK) "\u001B[0m")))))

(defn -main
  "Give me a CSV with API rest cases and I will verify them"
  [& args]
  (let [opts (apply hash-map (map-indexed #(if (even? %1) (subs %2 1) %2) (rest args)))]
    (println "running with arguments:" opts)
    (try
      (print-test-results (test-all (tests-from (first args) opts)))
      (finally
        (shutdown-agents)))))
