(ns fractl.subs.internal
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(defn- operation? [x]
  (when (some #{(u/string-as-keyword x)} [:create :update :delete])
    true))

(defn notification-object
  ([opr inst old-inst]
   (when-not (operation? opr)
     (u/throw-ex (str "invalid operation - " opr)))
   {:operation opr
    :instance (cn/make-instance inst)
    :old-instance (when old-inst (cn/make-instance inst))})
  ([opr inst] (notification-object opr inst nil)))

(defn notification-object? [obj]
  (and (map? obj)
       (map? (:instance obj))
       (operation? (:operation obj))))

(defn normalize-notification-object [obj]
  (let [instance (:instance obj)
        old-instance (:old-instance obj)
        opr (u/string-as-keyword (:operation obj))]
      (notification-object opr instance old-instance)))

(defn process-notification [obj]
  (when-let [r (rg/resolver-for-path (cn/instance-type (:instance obj)))]
    (r/call-resolver-on-change-notification r nil obj)))
