(ns rester.utils
  (:require [clojure.spec.alpha :as s]
            [rester.specs :as rs :refer [http-verbs]]
            [yaml.core :as yaml]
            [dk.ative.docjure.spreadsheet
             :refer
             [load-workbook read-cell row-seq select-sheet sheet-seq]]
            [clojure.string :as str]))

(def ^:const fields [:suite :name :url :verb :headers :payload :params :exp-status :exp-body
                     :exp-headers :options :extractions :priority])

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

(defn- to-test-case
  "convert yaml/json to test-case format"
  [params]
  (if-let [[verb url] (some #(some->> (params %) (conj [%])) http-verbs)]
    (let [data (-> params (assoc :verb verb :url url) (dissoc verb))
          parsed (s/conform ::rs/test-case data)]
      (if (= parsed ::s/invalid)
        {:error (s/explain-str ::rs/test-case data)}
        data))
    {:error (str "missing request verb/url:" (:suite params) "/" (:name params))}))

(defn- rows->test-cases
  "converts rows into test-cases"
  [rows]
  (loop [suite "(Ungrouped Tests)"
         id 0
         row (first rows)
         tests []]
    (let [t (zipmap fields row)
          suite (or (:suite t) suite)]
      (conj tests  (-> t (assoc :suite suite :id id )
                       (update :priority to-int)
                       (dissoc :exp-status :exp-body :exp-headers)
                       (assoc :expect
                              (into {} [[:status (:exp-status t)]
                                        (cond->> (:exp-body t)
                                          (comp not str/blank?) (vector :body))
                                        (cond->> (:exp-headers t)
                                          (comp not str/blank?) #(vector :headers (str->map % #":")))]))))))
  (for [row [rows]])
  (->> xs (map #(zipmap fields %)))
  (filter #(not (str/blank? (:test %))))
  (map #(update % :priority to-int))

  )

(defn from-yaml
  "Parses tests from a yaml file"
  [path]
  (apply concat
         (for [[suite tests] (yaml/from-file path)]
           (for [[i [test-name params]] (map-indexed vector tests)
                 :when (not= test-name :_)]
             (to-test-case (assoc params :id i :suite (name suite)
                                  :name (name test-name)))))))

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
                  rest))))

(defmethod load-tests-from :csv [file _]
  (with-open [in-file (io/reader file)]
    (doall (rest (csv/read-csv in-file)))))

(defmethod load-tests-from :yaml [file]
  (from-yaml file))

(defn parse-options [options]
  (into {} (for [opt (str/split options #"\s*,\s*")
                 :let[[_ key _ _ value] (re-find #"\s*([^:=\s]+)\s*((:|=)\s*([^\s]+))?\s*" opt) ]
                 :when key]
             [(keyword (str/lower-case key)) (or value true)])))

(defn- to-int [s]
  (try
    (if (empty? s) 0
        (Integer/parseInt s))
    (catch Exception e 0)))

(defn placeholders-of [{:keys[url headers params payload exp-body]}]
  (apply union (map placeholders [url headers params payload exp-body])))

(defn cyclic?
  ([deps x] (cyclic? deps x #{}))
  ([deps x visited]
   (or (and (visited x) x)
       (some #(cyclic? deps % (conj visited x)) (deps x)))))

(defn tests-from [file sheet]
  (let [tests (->> (map #(zipmap fields %) (read-rows file sheet))
                   (filter #(not (str/blank? (:test %))))
                   (map #(update % :priority to-int))
                   (map-indexed #(-> %2 (assoc :placeholders (placeholders-of %2) :id %1)
                                     (update :extractions str->map #"\s*=\s*"))))
        extractors (for [t tests :when (:extractions t)] [(:id t) (map first (:extractions t))])
        tests (map (fn [{:keys[placeholders options] :as test}]
                     (assoc test :deps (for [[i extractions] extractors
                                             :when (some placeholders extractions)] i)
                            :options (parse-options options))) tests)
        tests-with-deps (into {} (filter #(if (:deps %) [(:id %) (:deps %)]) tests))]
    (map #(if (cyclic? tests-with-deps (:id %)) (assoc % :error "circular dependency") %) tests)))
