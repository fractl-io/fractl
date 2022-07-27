(ns fractl.resolver.ui.core
  (:require [fractl.resolver.ui.application]
            [fractl.resolver.ui.table]
            [fractl.resolver.ui.instance]
            [fractl.resolver.ui.input-form]
            [fractl.resolver.registry :as rg]))

(defn set-default-ui-resolvers []
  (rg/register-resolvers
   [{:id :application-view
     :type :ui-application
     :paths [:Fractl.UI/Application]}
    {:id :input-form
     :type :ui-input-form
     :paths [:Fractl.UI/InputForm]}
    {:id :instance
     :type :ui-instance
     :paths [:Fractl.UI/InstanceForm]}
    {:id :table
     :type :ui-table
     :paths [:Fractl.UI/Table :Fractl.UI/Dashboard]}]))
