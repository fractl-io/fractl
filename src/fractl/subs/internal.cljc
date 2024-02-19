(ns fractl.subs.internal
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))
  
(defn notification-object? [obj]
  (and (map? obj)
       (map? (:instance obj))
       (some #{(:operation obj)} [:create :update :delete])
       true))

(defn process-notification [obj]
  (println "#######################" obj (notification-object? obj))
  (if (notification-object? obj)
    (let [instance (cn/make-instance (:instance obj))
          old-instance (when-let [old (:old-instance obj)]
                         (cn/make-instance old))]
      (when-let [r (rg/resolver-for-path (cn/instance-type instance))]
        (r/call-resolver-on-change-notification r nil {:instance instance
                                                       :old-instance old-instance
                                                       :operation (:operation obj)})))
    (u/throw-ex (str "not a notification object: " obj))))
