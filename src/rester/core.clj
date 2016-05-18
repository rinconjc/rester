(ns rester.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure.data
             [csv :as csv]
             [json :as json]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]))

(binding [factory/*json-factory* (factory/make-json-factory
                                  {:allow-unquoted-field-names true})])

(defn coerce-to-json [json-str]
  (json/read-str json-str))

(defn tests-from [file]
  (with-open [in-file (io/reader file)]
    (reduce
     (fn[suites [suite test path verb payload params exp-status exp-body exp-headers]]
       (let [test-case {:test test :path path :verb verb
                        :payload payload :params params :exp-status exp-status
                        :exp-body exp-body :exp-headers exp-headers}]
         (if-not (str/blank? suite)
           (conj suites {:suite suite
                         :tests [test-case]})
           (update suites (dec (count suites)) update :tests conj test-case))))
     []
     (rest (csv/read-csv in-file)))))

(defn mk-request [base-url {:keys[test path verb params payload]}]
  (log/info "executing" test ": " verb " " path)
  (client/request {:url (str base-url path)
                   :method verb
                   :query-params (->> (str/split params #"\s*,\s*")
                                      (map #(str/split % #"=" 2))
                                      (into {}))
                   :body (coerce-to-json payload)}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
