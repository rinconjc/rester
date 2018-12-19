(ns rester.utils
  (:require [cheshire.core :as json]
            [cheshire.factory :as factory]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [dk.ative.docjure.spreadsheet
             :refer
             [load-workbook read-cell row-seq select-sheet sheet-seq]]
            [rester.specs :as rs :refer [http-verbs]]
            [yaml.core :as yaml])
  (:import java.lang.Exception
           java.text.SimpleDateFormat
           java.util.Calendar))

(def ^:const fields [:suite :name :url :verb :headers :payload :params :exp-status :exp-body
                     :exp-headers :options :extractions :priority])

(def ^:const placeholder-pattern #"\$(\p{Alpha}[^\$]*)\$")
(def ^:const date-operand-pattern #"\s*(\+|-)\s*(\d+)\s*(\p{Alpha}+)")
(def ^:const date-exp-pattern #"(\p{Alpha}+)((\s*(\+|-)\s*\d+\s*\p{Alpha}+)*)(:(.+))?")
(def ^:const date-fields {"days" Calendar/DATE "day" Calendar/DATE "week" Calendar/WEEK_OF_YEAR
                          "weeks" Calendar/WEEK_OF_YEAR "year" Calendar/YEAR "years" Calendar/YEAR
                          "month" Calendar/MONTH "months" Calendar/MONTH
                          "hour" Calendar/HOUR "hours" Calendar/HOUR
                          "min" Calendar/MINUTE "mins" Calendar/MINUTE
                          "sec" Calendar/SECOND "secs" Calendar/SECOND})

(defn- to-int [s]
  (try
    (if (empty? s) 0
        (Integer/parseInt s))
    (catch Exception e 0)))

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

(defn parse-options [options]
  (into {} (for [opt (str/split options #"\s*,\s*")
                 :let[[_ key _ _ value] (re-find #"\s*([^:=\s]+)\s*((:|=)\s*([^\s]+))?\s*" opt) ]
                 :when key]
             [(keyword (str/lower-case key)) (or value true)])))

(defn replace-opts [s opts]
  (if (string? s)
    (str/replace s placeholder-pattern
                 #(str (or (opts (second %) (parse-date-exp (second %)))
                           (log/error "missing argument:" (second %)) "")))
    s))

(defn str->map
  ([s sep] (str->map s sep false))
  ([s sep include-empty]
   (if-not (str/blank? s)
     (let [[sep1 sep] (if (vector? sep) sep [#"\s*,\s*" sep])]
       (->> (str/split s sep1)
            (map #(let [[k v] (str/split % sep 2)
                        v (or v (log/warnf "missing key or value in %s" s))]
                    [k v]))
            (filter #(or include-empty (not (str/blank? (second %)))))
            (into {}))))))

(defn to-test-case
  "convert yaml/json to test-case format"
  [params]
  (if-let [[verb url] (some #(some->> (params %) (conj [%])) http-verbs)]
    (let [data (-> params
                   (assoc :verb verb :url url)
                   (dissoc verb))
          parsed (s/conform ::rs/test-case data)]
      (when (= parsed ::s/invalid)
        (throw (ex-info (format "invalid test case: %s" (:name params))
                        {:error (s/explain-str ::rs/test-case data)})))
      parsed)
    (throw (ex-info "missing request verb: url" (select-keys params [:suite :name])))))

(defn from-yaml
  "Parses tests from a yaml file"
  [path]
  (apply concat
         (for [[suite tests] (yaml/from-file path)]
           (for [[i [test-name params]] (map-indexed vector tests)
                 :when (not= test-name :_)]
             (to-test-case (assoc params :id i :suite (name suite)
                                  :name (name test-name)))))))

(defn- format-test [t]
  (-> t
      (dissoc :exp-status :exp-body :exp-headers :extractors)
      (update :headers str->map #":")
      (update :params str->map [#"&|(\s*,\s*)" #"\s*=\s*"] true)
      (assoc :expect
             (into {} [[:status (some-> (:exp-status t) Double/parseDouble .intValue)]
                       (some->> t :exp-body not-empty (vector :body))
                       (some->> t :exp-headers not-empty #(str->map % #":")
                                (vector :headers))]))
      (assoc :options
             (into (or (some-> t :options not-empty parse-options) {})
                   [[:priority (some-> t :priority not-empty to-int)]
                    (some->> t :extractors not-empty #(str->map #"\s*=\s*") (vector :extractors))]))))

(defn- rows->test-cases
  "converts rows into test-cases"
  [rows]
  (loop [suite "(Ungrouped Tests)"
         id 0
         rows rows
         tests []]
    (if (empty? rows)
      tests
      (let [t (zipmap fields (first rows))
            suite (or (:suite t) suite)
            t (format-test (assoc t :suite suite :id id))
            conformed (s/conform ::rs/test-case t)]
        (when (= conformed ::s/invalid)
          (throw (ex-info (format "invalid test case: %s" (:name t)) {:error (s/explain-str ::rs/test-case t)})))
        (recur suite (inc id) (rest rows) (conj tests conformed))))))

(defmulti load-tests-from
  "Load test cases from the given file"
  (fn [file _]
    (cond
      (str/ends-with? file ".csv") :csv
      (str/ends-with? file ".xlsx") :excel
      (str/ends-with? file ".yaml") :yaml)))

(defmethod load-tests-from :excel [file sheet]
  (let [wk-book (load-workbook file)
        wk-sheet (cond
                   (string? sheet) (select-sheet sheet wk-book)
                   :else (-> wk-book sheet-seq first))]
    (into [] (->> wk-sheet row-seq
                  (map #(map (fn[i] (if-let [c (.getCell % i)] (str (read-cell c)) ""))
                             (range (count fields))))
                  rest
                  rows->test-cases))))

(defmethod load-tests-from :csv [file _]
  (with-open [in-file (io/reader file)]
    (doall (rows->test-cases (rest (csv/read-csv in-file))))))

(defmethod load-tests-from :yaml [file _]
  (from-yaml file))

(defn parse-vars [s]
  (set (map second (re-seq placeholder-pattern s))))

(defn vars-in [{:keys[url headers params body expect]}]
  (->> [url (vals headers) (vals params)
        body (:body expect) (vals (:headers expect))]
       flatten
       (map parse-vars)
       (apply union)))

(defn cyclic?
  ([deps x] (cyclic? deps x #{}))
  ([deps x visited]
   (if (visited x) x
       (some #(cyclic? deps % (conj visited x)) (deps x)))))

(defn priority-dependencies
  "return map of priority to precedents. e.g. priority 1 depends on tests [3 4 5]"
  [tests]
  (loop [groups (->> tests
                     (group-by (comp :priority :options))
                     (remove (comp nil? first))
                     (sort-by first))
         result {}]
    (if (<= (count groups) 1)
      result
      (let [deps (->> groups first second (map (partial map :id)))
            priority (->> groups second first)]
        (recur (rest groups) (assoc result priority deps))))))

(defn get-by-name [tests]
  (let [tests (group-by :name tests)]
    (fn [n]
      (let [t (tests n)]
        (when (nil? t) (log/warnf "Referenced test with name [%s] is not defined" n))
        (when (> (count t) 1) (log/warnf "Ambiguous reference to multiple test with name [%s]" n))
        (first t)))))

(defn process-tests [tests opts]
  (let [skip-tag (opts "skip")
        by-name (get-by-name tests)
        tests (for [t tests :let [ignored (-> t :options :ignore)
                                  skipped (and skip-tag (= skip-tag (-> t :options :skip)))]]
                (cond-> t ignored (assoc :ignored true) skipped (assoc :skipped true)))
        runnables (into [] (comp (remove :ignored)
                                 (remove :skipped)
                                 (map #(assoc :vars (vars-in %)))) tests)
        extractors (->> (for [{:keys[id options]} runnables :when (:extractors options)]
                          (map #(vector (first %) id) (:extractors options)))
                        flatten
                        (into {}))      ;var->id
        priority-deps (priority-dependencies runnables)
        runnables (for [t runnables]
                    (-> (assoc :deps (-> extractors
                                         (select-keys (:vars t))
                                         (concat (priority-deps (-> t :options :priority)))
                                         distinct))
                        (update-in [:options :before] (partial map by-name))
                        (update-in [:options :after] (partial map by-name))))
        tests-with-deps (into {} (filter #(if (:deps %) [(:id %) (:deps %)]) runnables))]

    (when-let [invalid-test (some #(if (cyclic? tests-with-deps (:id %)) %) runnables)]
      (throw (Exception. (format "cyclic dependency in test %s" (:name invalid-test)))))
    {:runnable runnables
     :ignored (filter :ignored tests)
     :skipped (filter :skipped tests)}))
