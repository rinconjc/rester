(ns rester.utils
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [dk.ative.docjure.spreadsheet
             :refer
             [load-workbook read-cell row-seq select-sheet sheet-seq]]
            [rester.specs :as rs :refer [http-verbs]]
            [yaml.core :as yaml])
  (:import java.io.PushbackReader
           java.lang.Exception
           java.text.SimpleDateFormat
           java.util.Calendar))

(def ^:const fields [:suite :name :url :verb :headers :body :params :exp-status :exp-body
                     :exp-headers :options :extractors :priority])

(def ^:const placeholder-pattern #"\$(\p{Alpha}[^\$]*)\$")
(def ^:const date-operand-pattern #"\s*(\+|-)\s*(\d+)\s*(\p{Alpha}+)")
(def ^:const date-exp-pattern #"(\p{Alpha}+)((\s*(\+|-)\s*\d+\s*\p{Alpha}+)*)(:(.+))?")
(def ^:const date-fields {"days" Calendar/DATE "day" Calendar/DATE "week" Calendar/WEEK_OF_YEAR
                          "weeks" Calendar/WEEK_OF_YEAR "year" Calendar/YEAR "years" Calendar/YEAR
                          "month" Calendar/MONTH "months" Calendar/MONTH
                          "hour" Calendar/HOUR "hours" Calendar/HOUR
                          "min" Calendar/MINUTE "mins" Calendar/MINUTE
                          "sec" Calendar/SECOND "secs" Calendar/SECOND})

(defn like [x y]
  (cond
    (map? x) (and (map? y)
                  (every? (fn[[k v]] (if (fn? v) (v (get y k)) (like v (get y k)))) x))
    (coll? x) (and (coll? y) (or (and (empty? x) (empty? y))
                                 (and (like (first x) (first y)) (like (rest x) (rest y)))))
    (fn? x) (x y)
    :else (= x y)))

(defn deep-merge [a b]
  (merge-with #(if (map? %1) (deep-merge %1 %2) %2) a b))

(defn- map-keys [f m]
  (reduce-kv #(assoc %1 (f %2) %3) {} m))

(defn- map-values [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn- to-int [s]
  (try
    (if (empty? s) 0
        (int (Double/parseDouble s)))
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
  (cond
    (string? s) (str/replace s placeholder-pattern
                             #(str (or (replace-opts (opts (second %) (parse-date-exp (second %))) opts)
                                       (log/error "missing argument:" (second %)) "")))
    (sequential? s) (map #(replace-opts % opts) s)
    :else s))

(defn str->map [s sep & {:keys[include-empty grouped]}]
  (when-not (str/blank? s)
    (let [[sep1 sep] (if (vector? sep) sep [#"\s*,\s*" sep])
          pairs (->> (str/split s sep1)
                     (map #(let [[k v] (str/split % sep 2)
                                 v (or v (log/warnf "missing key or value in %s" s))]
                             [k v]))
                     (filter #(or include-empty (not (str/blank? (second %))))))]
      (if grouped
        (->> pairs (group-by first) (map-values #(map second %)))
        (into {} pairs)))))

(defn to-test-case
  "convert yaml/json to test-case format"
  [params]
  (if-let [[verb url] (some #(some->> (params %) (conj [%])) http-verbs)]
    (let [data (-> params
                   (assoc :verb verb :url url)
                   (dissoc verb :priority :extract :ignore :skip)
                   (update :headers (partial map-keys name))
                   (update-in [:expect :headers] (partial map-keys name))
                   (assoc :options (-> params (select-keys [:priority :ignore :skip :before :after :parse-body])))
                   (assoc-in [:options :extractors] (map-keys name (:extract params))))
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
        (update :headers str->map [#"\s*,\s*(?=[^,]*:)" #":"])
        (update :params str->map [#"&|(\s*,\s*)" #"\s*=\s*"] :include-empty true :grouped true)
        (assoc :expect
               (into {} [[:status (some-> (:exp-status t) Double/parseDouble .intValue)]
                         (some->> t :exp-body not-empty (vector :body))
                         (some->> t :exp-headers (#(str->map % [#"\s*,\s*(?=[^,]*:)" #":"]))
                                  (vector :headers))]))
        (assoc :options
               (into (or (some-> t :options not-empty parse-options
                                 (#(merge % (some->>
                                             (select-keys % [:before :after])
                                             (map-values (fn[s](str/split s #"\s*,\s*")))))))
                         {})
                     [(some->> t :priority ((fnil to-int "0")) (vector :priority))
                      (some->> t :extractors not-empty (#(str->map % #"\s*=\s*"))
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
  (fn [file _]
    (cond
      (str/ends-with? file ".csv") :csv
      (str/ends-with? file ".xlsx") :excel
      (str/ends-with? file ".yaml") :yaml
      (str/ends-with? file ".edn") :edn
      :else (throw (ex-info "file type not supported." {})))))

(defmethod load-tests-from :excel [file sheet]
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

(defn parse-vars [s]
  (when s (set (map second (re-seq placeholder-pattern s)))))

(defn vars-in [{:keys[url headers params body expect]}]
  (->> [url (vals headers) (vals params)
        body (:body expect) (vals (:headers expect))]
       flatten
       (map parse-vars)
       (apply union)))

(defn find-cycle
  ([deps x] (find-cycle deps x #{}))
  ([deps x visited]
   (if (visited x) visited
       (some #(find-cycle deps % (conj visited x)) (deps x)))))

(defn priority-dependencies
  "return map of priority to precedents. e.g. priority 1 depends on tests [3 4 5]"
  [tests]
  (loop [groups (->> tests
                     (filter (comp :priority :options))
                     (group-by (comp :priority :options))
                     (sort-by first))
         result {}]
    (if (<= (count groups) 1)
      result
      (let [deps (->> groups first second (map :id))
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
  (let [skip-tag (opts :skip)
        common-headers (:headers opts)
        by-name (get-by-name tests)
        tests (for [t tests :let [ignored (-> t :options :ignore)
                                  skipped (and skip-tag (= skip-tag (-> t :options :skip)))]]
                (cond-> t ignored (assoc :ignored true) skipped (assoc :skipped true)))
        runnables (into [] (comp (remove :ignored)
                                 (remove :skipped)
                                 (map #(assoc % :vars (vars-in %)
                                              :headers (merge common-headers (:headers %))))) tests)
        extractors (->> (for [{:keys[id options]} runnables
                              :when (:extractors options)]
                          (map #(vector (first %) id) (:extractors options)))
                        (apply concat) (into {}))      ;var->id
        priority-deps (priority-dependencies runnables)
        runnables (for [t runnables]
                    (-> t
                        (merge (->> (select-keys extractors (:vars t))
                                    vals set (repeat 2)
                                    (zipmap [:var-deps :deps])))
                        (update :deps set/union (-> t :options :priority priority-deps set))
                        (update-in [:options :before] (partial map by-name))
                        (update-in [:options :after] (partial map by-name))))
        tests-with-deps (reduce #(if (:deps %2) (conj %1 [(:id %2) (:deps %2)]) %1) {} runnables)]

    (when-let [deps (some #(find-cycle tests-with-deps (:id %)) runnables)]
      (throw (Exception. (format "cyclic dependency between tests:\n%s"
                                 (->> runnables (filter #(deps (:id %))) (map :name) (str/join "\n"))))))
    {:runnable runnables
     :ignored (filter :ignored tests)
     :skipped (filter :skipped tests)}))


(defn load-config [file]
  (let [config (->> file yaml/from-file
                    (map-values #(-> % (update :bindings (partial map-keys name))
                                     (update :headers (partial map-keys name)))))]
    (if (s/valid? ::rs/config config)
      config
      (throw (ex-info "Invalid configuration format" :error (s/explain ::rs/config config))))))
