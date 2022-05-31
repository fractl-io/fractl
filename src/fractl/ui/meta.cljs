(ns fractl.ui.meta
  (:require [fractl.policy :as policy]
            [fractl.component :as cn]))

(def ^:private views-tag :views)

(defn- views-meta [meta]
  (if (views-tag meta)
    meta
    (let [rec-name (cn/meta-of meta)]
      {views-tag (policy/lookup-policies views-tag rec-name)})))

(defn view-event [meta tag]
  (get-in meta (concat [views-tag] tag)))

(defn authorize? [meta]
  (= :authorize
     (get-in
      meta
      [views-tag :create-button :on-success])))

(defn contains [meta]
  (seq (get-in meta [views-tag :contains])))

(defn attribute-view-spec [meta field-name]
  (get-in meta [views-tag :attributes field-name :input]))

(defn create-button-label [meta]
  (get-in meta [views-tag :create-button :label]))

(def upsert-event :upsert-event)
(def delete-event :delete-event)
(def order :order)

(defn styles [meta]
  (get-in meta [views-tag :styles]))
