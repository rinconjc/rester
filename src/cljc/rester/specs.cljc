(ns rester.specs
  (:require #?(:clj [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])))

(def http-verbs #{:get :post :put :delete :patch :options} )
(s/def ::verb (s/and keyword? http-verbs))
(s/def ::verb-str (into #{} (map name http-verbs)))
(s/def ::headers (s/nilable (s/map-of string? string?)))
(s/def ::json-body (s/or :array seq? :object map?))
(s/def ::raw-body string?)
(s/def ::form-body (s/map-of string? string?))
(s/def ::url string?)
(s/def ::status int?)
(s/def ::priority int?)
(s/def ::extractors (s/map-of string? string?))
(s/def ::name string?)
(s/def ::suite string?)
(s/def ::parse-body boolean?)
(s/def ::ignore boolean?)
(s/def ::skip string?)
(s/def ::names (s/coll-of string?))
(s/def ::before ::names)
(s/def ::after ::names)
(s/def ::params (s/nilable (s/coll-of (s/tuple string? any?))))
;; (s/def ::body (s/or :raw ::raw-body :json ::json-body :form ::form-body))
(s/def ::body any?)               ;raw-body only for now
(s/def ::expect (s/keys :req-un [::status] :opt-un [::headers ::body]))
(s/def ::options (s/keys :opt-un [::priority ::extractors ::parse-body ::ignore
                                  ::skip ::before ::after]))

(s/def ::test-case (s/keys :req-un [::id ::suite ::name ::verb ::url ::expect]
                           :opt-un [::body ::headers ::options ::params]))

;; (s/def ::test-case-row
;;   (s/cat :suite string? :name string? :url string? :verb ::verb-str :headers ))
(s/def ::bindings (s/map-of string? any?))
(s/def ::report string?)
(s/def ::skip string?)
(s/def ::concurrency (s/and int? #(< 0 % 1000)))
(s/def ::profile (s/keys :opt-un [::bindings ::headers ::report ::skip ::concurrency]))
(s/def ::config (s/map-of keyword? ::profile))
