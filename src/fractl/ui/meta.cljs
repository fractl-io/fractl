(ns fractl.ui.meta
  (:require [fractl.policy :as policy]
            [fractl.component :as cn]))

(def ^:private views-key :views)

(defn- views [meta]
  (or (views-key meta)
      (let [rec-name (cn/meta-of meta)]
        (policy/lookup-policies views-key rec-name))))

(defn view-event [meta tag]
  (tag (views meta)))

(defn authorize? [meta]
  (= :authorize
     (get-in
      (views meta)
      [:create-button :on-success])))

(defn contains [meta]
  (seq (:contains (views meta))))

(defn attribute-view-spec [meta field-name]
  (get-in (views meta) [:attributes field-name :input]))

(defn create-button-label [meta]
  (get-in (views meta) [:create-button :label]))

(def upsert-event :upsert-event)
(def delete-event :delete-event)
(def order :order)

(defn styles [meta]
  (:styles (views meta)))
