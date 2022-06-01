(ns fractl.meta)

(def meta-key :-*-meta-*-)
(def meta-of-key :-*-meta-of-*-)
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

(def upsert-event :upsert-event)
(def delete-event :delete-event)
(def order :order)
(def contains :contains)
