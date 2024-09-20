(ns agentlang.datafmt.json
  (:require #?(:clj [cheshire.core :as json])
            [clojure.string :as s]))

(defn encode [obj]
  #?(:clj (json/generate-string obj)
     :cljs (.stringify js/JSON (clj->js obj))))

(def ^:private space-pat #"\s")

(defn- keywordify [k]
  (if (and (string? k) (not (re-find space-pat k)))
    (let [k (if (s/starts-with? k ":") (subs k 1) k)]
      (keyword k))
    k))

(defn decode [s]
  #?(:clj (json/parse-string s keywordify)
     :cljs (.parse js/JSON s)))
