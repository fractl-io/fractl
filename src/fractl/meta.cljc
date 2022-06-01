(ns fractl.meta
  (:require [fractl.policy :as policy]
            [fractl.component :as cn]))

(def ^:private views-tag :views)

(defn- views-spec [meta]
  (if-let [spec (views-tag meta)]
    spec
    (let [rec-name (cn/meta-of meta)]
      (policy/spec (first (policy/lookup-policies views-tag rec-name))))))

(defn views-event [meta tag]
  (get (views-spec meta) tag))

(defn views-authorize? [meta]
  (= :authorize
     (get-in
      (views-spec meta)
      [:create-button :on-success])))

(defn contains [meta]
  (seq (get (views-spec meta) :contains)))

(defn views-attribute-view-spec [meta field-name]
  (get-in (views-spec meta) [:attributes field-name :input]))

(defn views-create-button-label [meta]
  (get-in (views-spec meta) [:create-button :label]))

(def upsert-event :upsert-event)
(def delete-event :delete-event)
(def order :order)

(defn views-styles [meta]
  (get (views-spec meta) :styles))
