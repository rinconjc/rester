(ns rester.utils
  (:require [clojure.spec.alpha :as s]
            [rester.specs :as rs :refer [http-verbs]]
            [yaml.core :as yaml]))

(defn to-test-case [params]
  (if-let [[verb url] (some #(some->> (params %) (conj [%])) http-verbs)]
    (let [data (-> params (assoc :verb verb :url url) (dissoc verb))
          parsed (s/conform ::rs/test-case data)]
      (if (= parsed ::s/invalid)
        {:error (s/explain-str ::rs/test-case data)}
        data))
    {:error (str "missing request verb/url:" (:suite params) "/" (:name params))}))

(defn from-yaml [path]
  (for [[suite tests] (yaml/from-file path)]
    (for [[test-name params] tests
          :when (not= test-name :_)]
      (to-test-case (assoc params :suite (name suite) :name (name test-name))))))
