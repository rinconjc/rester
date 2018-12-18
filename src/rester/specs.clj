(ns rester.specs
  (:require [clojure.spec.alpha :as s]))

(def http-verbs #{:GET :POST :PUT :PATCH :OPTIONS} )
(s/def ::verb http-verbs)
(s/def ::verb-str (into #{} (map name http-verbs)))
(s/def ::headers (s/map-of keyword? string?))
(s/def ::json-body (s/or :array seq? :object map?))
(s/def ::raw-body string?)
(s/def ::form-body (s/map-of string? string?))
(s/def ::url string?)
(s/def ::status int?)
(s/def ::priority int?)
(s/def ::extractors (s/map-of keyword? string?))
(s/def ::name string?)
(s/def ::suite string?)
(s/def ::parse-body boolean?)
(s/def ::ignore boolean?)
(s/def ::skip string?)
(s/def ::names (s/coll-of string?))
(s/def ::before ::names)
(s/def ::after ::names)
(s/def ::body (s/or :raw ::raw-body :json ::json-body :form ::form-body))
(s/def ::expect (s/keys :req-un [::status] :opt-un [::headers ::body]))
(s/def ::options (s/keys :opt-un [::priority ::extractors ::parse-body ::ignore
                                  ::skip ::before ::after]))

(s/def ::test-case (s/keys :req-un [::suite ::name ::verb ::url ::expect]
                           :opt-un [::body ::headers ::options]))

;; (s/def ::test-case-row
;;   (s/cat :suite string? :name string? :url string? :verb ::verb-str :headers ))
