(ns fractl.resolver.ui.application
  (:require [fractl.resolver.core :as rc]
            [fractl.ui.core :as ui]))

(defn- appinst-to-config [inst]
  (merge (:Config inst)
         {:view {:dashboard (:DashboardRecord inst)
                 :components (:Components inst)}}))

(defn upsert-ui [instance]
  (let [config (appinst-to-config instance)]
    (ui/init-view config (:PostInitEvent instance))))

(defn make [resolver-name]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
