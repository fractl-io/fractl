(ns fractl.ui.setup
  (:require [fractl.resolver.registry :as rg]
            [fractl.resolver.ui.application :as vapp]
            [fractl.resolver.ui.table :as vt]
            [fractl.resolver.ui.instance :as vi]
            [fractl.resolver.ui.input-form :as vif]))

(defn set-default-ui-resolvers []
  (rg/override-resolver
   [:Fractl.UI/Application]
   (vapp/make :application-view))
  (rg/override-resolver
   [:Fractl.UI/InputForm]
   (vif/make :input-form))
  (rg/override-resolver
   [:Fractl.UI/InstanceForm]
   (vi/make :instance))
  (rg/override-resolver
   [:Fractl.UI/Table :Fractl.UI/Dashboard]
   (vt/make :table)))

(set-default-ui-resolvers)
