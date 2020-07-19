(ns rester.utils
  (:require [clojure.set :as set :refer [union]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            #?(:clj [clojure.tools.logging :as clog])))

(defonce placeholder-pattern #"\$([a-zA-Z][^\$]*)\$")
(defonce date-exp-pattern #"(now|today|tomorrow)((\s*(\+|-)\s*\d+\s*(day|hour|min|sec|week|month|year)s?)*)(:(.+))?")
(defonce date-operand-pattern #"\s*(\+|-)\s*(\d+)\s*(day|hour|min|sec|week|month|year)s?")

(defn log [msg & more]
  #?(:clj (clog/warnf msg more)
     :cljs (js/console.log msg more)))

(defn try-some [x f & [g & more]]
  (try
    (f x)
    (catch #?(:clj Exception
              :cljs js/Error) e
      (log "failed applying" f "to" x " due to:" e)
      (if (fn? g)
        (apply try-some x g more)
        (throw e)))))

(defn grouped [xs key-fn val-fn]
  (reduce #(update %1 (key-fn %2) conj (val-fn %2)) {} xs))

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

(defn map-keys [f m]
  (reduce-kv #(assoc %1 (f %2) %3) {} m))

(defn map-values [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn str->tuples [s sep & {:keys[include-empty]}]
  (when-not (str/blank? s)
    (let [[sep1 sep] (if (vector? sep) sep [#"\s*,\s*" sep])]
      (->> (str/split s sep1)
           (map #(let [[k v] (str/split % sep 2)
                       v (or v (log "missing key or value in %s" s))]
                   [k v]))
           (filter #(or include-empty (not (str/blank? (second %)))))))))

(defn str->map [s sep & {:keys[grouped] :as opts}]
  (when-let [pairs (apply str->tuples s sep opts)]
    (if grouped
      (->> pairs (group-by first) (map-values #(map second %)))
      (into {} pairs))))

(defn parse-vars [s]
  (when s (set (map second (re-seq placeholder-pattern s)))))

(defn vars-in [{:keys[url headers params body expect]}]
  (->> [url (vals headers) (map second params)
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
        (when (nil? t) (log "Referenced test with name [%s] is not defined" n))
        (when (> (count t) 1) (log "Ambiguous reference to multiple test with name [%s]" n))
        (first t)))))

(defn exception [msg]
  #?(:clj (Exception. msg)
     :cljs (js/Error. msg)))

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
      (throw (exception (str "cyclic dependency between tests:\n"
                             (->> runnables (filter #(deps (:id %))) (map :name) (str/join "\n"))))))
    {:runnable runnables
     :ignored (filter :ignored tests)
     :skipped (filter :skipped tests)}))
