(ns fractl.datafmt.json
  #?(:clj (:require [cheshire.core :as json])))

(defn encode [obj]
  #?(:clj (json/generate-string obj)
     :cljs (clj->js obj)))

(def ^:private space-pat #"\s")

(defn- keywordify [k]
  (if (and (string? k) (not (re-find space-pat k)))
    (keyword k)
    k))

(defn decode [s]
  #?(:clj (json/parse-string s keywordify)
     :cljs (.parse js/JSON s)))
