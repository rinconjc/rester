(ns rester.core
  (:gen-class)
  (:require [clj-http.cookies :as cookies]
            [clj-http.core :as http]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [clojure.tools.logging :as log]
            [rester.impl
             :refer
             [exec-in-order junit-report print-test-results summarise-results]]
            [rester.utils :as utils
             :refer
             [deep-merge load-config process-tests]])
  (:import java.io.File
           java.lang.Integer
           java.util.Date))

(defn exec-tests [tests opts]
  (let [test-groups (process-tests tests opts)
        executed (exec-in-order (:runnable test-groups) opts)]
    (apply concat executed (-> test-groups (dissoc :runnable) vals))))

(defn load-tests [file opts]
  (utils/load-tests-from file opts))

(defn run-tests [file opts]
  (let [[tests-file work-sheet] (str/split file #":" 2)]
    (binding [http/*cookie-store* (when (:use-cookies opts) (cookies/cookie-store))]
      (-> tests-file
          (load-tests work-sheet)
          (exec-tests opts)
          (summarise-results)))))

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
   ["-k" "--use-cookies" "Save and send cookies automatically"]
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

    (try
      (doseq [p (-> opts :options :profiles)
             f (:arguments opts)
             :let [options (deep-merge (get-in opts [:options :config (keyword p)])
                                       (-> opts :options (dissoc :config)))
                   start (System/currentTimeMillis)
                   _ (log/infof "Executing %s with profile %s : %s" f p (Date.))
                   results (run-tests f options)]]
       (log/infof "Completed in %f secs" (/ (- (System/currentTimeMillis) start) 1000.0))
       ((juxt #(junit-report (:options opts) f %) print-test-results) results))
      (catch Exception e
        (log/fatalf e "Failed executing tests!")
        (System/exit 1)))))
