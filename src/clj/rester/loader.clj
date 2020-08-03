(ns rester.loader
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [dk.ative.docjure.spreadsheet
             :refer
             [load-workbook read-cell row-seq select-sheet sheet-seq]]
            [rester.specs :as rs :refer [http-verbs]]
            [yaml.core :as yaml]
            [rester.utils :as utils])
  (:import java.io.PushbackReader))

(def ^:const fields [:suite :name :url :verb :headers :body :params :exp-status :exp-body
                     :exp-headers :options :extractors :priority])

(defn- to-int [s]
  (try
    (if (empty? s) 0
        (int (Double/parseDouble s)))
    (catch Exception e 0)))

(defn parse-options [options]
  (into {} (for [opt (str/split options #"\s*,\s*")
                 :let[[_ key _ _ value] (re-find #"\s*([^:=\s]+)\s*((:|=)\s*([^\s]+))?\s*" opt) ]
                 :when key]
             [(keyword (str/lower-case key)) (or value true)])))

(defn to-test-case
  "convert yaml/json to test-case format"
  [params]
  (if-let [[verb url] (some #(some->> (params %) (conj [%])) http-verbs)]
    (let [data (-> params
                   (assoc :verb verb :url url)
                   (dissoc verb :priority :extract :ignore :skip)
                   (update :headers (partial utils/map-keys name))
                   (update-in [:expect :headers] (partial utils/map-keys name))
                   (assoc :options (-> params (select-keys [:priority :ignore :skip :before :after :parse-body])))
                   (assoc-in [:options :extractors] (utils/map-keys name (:extract params))))
          parsed (s/conform ::rs/test-case data)]
      (when (= parsed ::s/invalid)
        (log/warn "invalid test case:" (s/explain-data ::rs/test-case data))
        (throw (ex-info (format "invalid test case: %s" (:name data))
                        {:error (s/explain-data ::rs/test-case data)})))
      parsed)
    (throw (ex-info "missing request verb: url" (select-keys params [:suite :name])))))

(defn from-yaml
  "Parses tests from a yaml file"
  [path]
  (->> (yaml/from-file path)
       (map (fn[[suite ts]]
              (map (fn[[k t]]
                     (when (not= :_ k) (assoc t :name (name k) :suite (name suite)))) ts)))
       (apply concat)
       (filter some?)
       (map-indexed #(to-test-case (assoc %2 :id %1)))))

(defn- format-test [t]
  (try
    (-> t
        (dissoc :exp-status :exp-body :exp-headers :extractors)
        (update :verb (comp keyword str/lower-case))
        (update :headers utils/str->map [#"\s*,\s*(?=[^,]*:)" #":"])
        (update :params utils/str->tuples [#"&|(\s*,\s*)" #"\s*=\s*"] :include-empty true )
        (assoc :expect
               (into {} [[:status (some-> (:exp-status t) Double/parseDouble .intValue)]
                         (some->> t :exp-body not-empty (vector :body))
                         (some->> t :exp-headers (#(utils/str->map % [#"\s*,\s*(?=[^,]*:)" #":"]))
                                  (vector :headers))]))
        (assoc :options
               (into (or (some-> t :options not-empty parse-options
                                 (#(merge % (some->>
                                             (select-keys % [:before :after])
                                             (utils/map-values (fn[s](str/split s #"\s*,\s*")))))))
                         {})
                     [(some->> t :priority ((fnil to-int "0")) (vector :priority))
                      (some->> t :extractors not-empty (#(utils/str->map % #"\s*=\s*"))
                               (vector :extractors))])))
    (catch Exception e
      (log/error e "failed parsing test:" t)
      (throw (ex-info (str "failed parsing test:" t) {:test t})))))

(defn rows->test-cases
  "converts rows into test-cases"
  [rows]
  (loop [suite "(Ungrouped Tests)"
         id 0
         rows rows
         tests []]
    (if (empty? rows)
      tests
      (let [t (zipmap fields (first rows))
            suite (or (not-empty (:suite t)) suite)
            t (format-test (assoc t :suite suite :id id))
            conformed (s/conform ::rs/test-case t)]
        (when (= conformed ::s/invalid)
          (throw (ex-info (format "invalid test case: %s" (:name t))
                          {:error (s/explain-data ::rs/test-case t)})))
        (recur suite (inc id) (rest rows) (conj tests conformed))))))

(defmulti load-tests-from
  "Load test cases from the given file"
  (fn [file-name opts]
    (println "opts::..." opts)
    (condp #(str/ends-with? %2 %1) (or (:filename opts) file-name)
      ".csv" :csv
      ".xlsx" :excel
      ".yaml" :yaml
      ".edn" :edn
      (throw (ex-info "file type not supported!" {:file file-name})))))

(defmethod load-tests-from :excel [file {sheet :sheet}]
  (let [wk-book (load-workbook file)
        wk-sheet (cond
                   (string? sheet) (select-sheet sheet wk-book)
                   :else (-> wk-book sheet-seq first))]
    (into [] (->> wk-sheet row-seq
                  (map #(map (fn[i] (if-let [c (.getCell % i)] (str (read-cell c)) ""))
                             (range (inc (count fields)))))
                  rest
                  rows->test-cases))))

(defmethod load-tests-from :csv [file _]
  (with-open [in-file (io/reader file)]
    (doall (rows->test-cases (rest (csv/read-csv in-file))))))

(defmethod load-tests-from :yaml [file _]
  (from-yaml file))

(defmethod load-tests-from :edn [file _]
  (->> (edn/read (PushbackReader. (io/reader file)))
       (mapcat (fn[[suite tests]]
                 (map (fn[[name* test]]
                        (assoc test :suite suite :name name*)) tests)))
       (map-indexed #(to-test-case (assoc %2 :id %1)))))

(defmulti exporter identity)

(defn- join-map [ed pd m]
  (str/join ed (map (partial str/join pd) m)))

(defmethod exporter :csv []
  (fn [tests writer]
    (csv/write-csv writer (->> fields (map name) (map str/upper-case)))
    (loop [prev-suite nil
           [test & more] tests]
      (when test
        (csv/write-csv writer (juxt #(if (= prev-suite (:suite %)) "" (:suite %))) 
                       :name :url :verb
                       (comp (partial join-map "," ":") :headers)
                       :body (comp (partial join-map "&" "=")) 
                       (comp :status :expect)
                       (comp :body :expect)
                       (comp (partial join-map "," ":") :headers :expect)
                       (comp (partial join-map "," "=") #(dissoc % :extractors :priority) :options)
                       (comp (partial join-map "\n" "=") :extractors :options)
                       (comp :priority :options))
        (recur (:suite test) more)))))

(defmethod exporter :yaml []
  (fn [tests writer]
    (spit writer (yaml/generate-string tests {:flow-style :block}) )))

(defmethod exporter :edn []
  (fn [tests writer]
    (spit writer (pr-str tests))))

(defn export [tests format writer]
  (let [exporter (exporter format)]
    (exporter tests writer)))
