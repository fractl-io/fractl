(ns fractl.resolver.ui.application
  (:require [fractl.resolver.core :as rc]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.ui.core :as ui]))

(defn- appinst-to-config [inst]
  (merge (:Config inst)
         {:view {:dashboard (:DashboardRecord inst)
                 :components (:Components inst)}}))

(defn upsert-ui [instance]
  (let [config (appinst-to-config instance)]
    (ui/init-view config (:PostInitEvent instance))))

(defnake :ui-application
  (fn [resolver-name _]
    (rc/make-resolver
     resolver-name
     {:upsert {:handler upsert-ui}})))
