(ns fractl.ui.config
  (:require [fractl.global-state :as gs]
            [fractl.meta :as mt]
            [fractl.policy :as policy]))

(defn- views-spec [rec-name]
  (let [spec (policy/spec
              (first (policy/lookup-policies mt/views-tag rec-name)))]
    (if (map? spec)
      spec
      (second spec))))

(defn views-event [rec-name tag]
  (get (views-spec rec-name) tag))

(defn views-authorize? [rec-name]
  (= :authorize
     (get-in
      (views-spec rec-name)
      [:create-button :on-success])))

(defn views-attribute-view-spec [rec-name field-name]
  (get-in (views-spec rec-name) [:attributes field-name :input]))

(defn views-create-button-label [rec-name]
  (get-in (views-spec rec-name) [:create-button :label]))

(defn views-styles [rec-name]
  (get (views-spec rec-name) :styles))

(defn dashboard
  ([config]
   (get-in config [:view :dashboard]))
  ([] (dashboard (gs/get-app-config))))

(defn component
  ([config]
   (get-in config [:view :component]))
  ([] (component (gs/get-app-config))))
