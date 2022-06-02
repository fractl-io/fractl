(ns fractl.meta
  (:require [fractl.util.seq :as su]))

(def meta-key :-*-meta-*-)
(def meta-of-key :-*-meta-of-*-)

(def upsert-event :upsert-event)
(def delete-event :delete-event)
(def order :order)
(def contains :contains)

(def ^:private custom-parsers (atom {}))

(def views-tag :views)
(def ^:private policy-tags [views-tag])

(defn meta-as-policies [rec-name meta]
  (seq
   (filter
    identity
    (mapv (fn [[k v]]
            (when (some #{k} policy-tags)
              [k rec-name v]))
          meta))))

(def policy-parsers :policy)

(defn set-policy-parser! [intercept f]
  (let [ps (policy-parsers @custom-parsers)
        ips (get ps intercept [])]
    (swap!
     custom-parsers assoc
     policy-parsers
     (assoc ps intercept (conj ips f)))))

(defn- get-policy-parsers [intercept]
  (intercept (policy-parsers @custom-parsers)))

(defn apply-policy-parsers [rec-name meta]
  (su/nonils
   (mapv (fn [[intercept _ _ :as args]]
           (when-let [fs (get-policy-parsers intercept)]
             (apply concat (mapv #(apply % args) fs))))
         (meta-as-policies rec-name meta))))
