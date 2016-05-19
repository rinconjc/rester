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
            [clojure.tools.logging :as log]))

(defn json->clj [json-str]
  (if-not (str/blank? json-str)
    (binding [factory/*json-factory* (factory/make-json-factory
                                      {:allow-unquoted-field-names true})]
      (json/parse-string json-str))))

(defn str->map [s sep]
  (if-not (str/blank? s)
    (->> (str/split s #"\s*,\s*") (map #(str/split % sep 2)) (into {}))))

(defn tests-from [file]
  (with-open [in-file (io/reader file)]
    (reduce
     (fn[suites [suite test path verb headers payload params exp-status exp-body exp-headers]]
       (let [test-case {:test test :path path :verb verb
                        :headers (str->map headers #":")
                        :params (str->map params #"\s*=\s*")
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

(defn mk-request [base-url {:keys[test path verb headers params payload]}]
  (log/info "executing" test ": " verb " " path)
  (client/request {:url (str base-url path)
                   :method (keyword (str/lower-case verb))
                   :headers headers
                   :query-params params
                   :body payload}))

(defn verify-response [{:keys [status body headers] :as resp}
                       {:keys[exp-status exp-headers exp-body]}]

  (let [body* (json->clj body)]
    (or
     (and (not= status exp-status)
          (str "status " status " not equal to expected " exp-status))
     (some #(fn [[header value]]
              (if (not= value (headers header))
                (str "header " header " was " (headers header) " expected " value))) exp-headers)
     (if-let [diff (and (not-empty exp-body) (first (data/diff exp-body body*)))]
       (->> diff json/generate-string (str "expected body missing:" ))))))

(defn test-all [base-url suites]
  (for [{:keys[suite tests]} suites]
    {:suite suite
     :results (map #(vector (% :test)
                            (or (verify-response (mk-request base-url %) %) :OK)) tests)}))

(defn print-test-results [suites]
  (println "=============================================================")
  (doseq [{:keys [suite results]} suites]
    (println suite)
    (doseq [[test result] results]
      (print "...")
      (print test (apply str (repeat (- 20 (count test)) ".")))
      (println result))))

(defn -main
  "Give me a CSV with API rest cases and I will verify them"
  [& args]
  (print-test-results
   (doall (test-all (first args) (tests-from (second args))))))
