(ns fractl.ui.model
  (:require [fractl.lang
             :refer [component entity dataflow]]))

(component :Fractl.UI)

(defn fields? [xs]
  (every? #(or (string? %) (keyword? %)) xs))

(entity
 :Fractl.UI/InputForm
 {:Record :Kernel/Path
  :Fields {:check fields?}
  :View {:type :Kernel/Any
         :optional true}})

(entity
 :Fractl.UI/InstanceForm
 {:Record :Kernel/Path
  :Fields {:check fields?}
  :View {:type :Kernel/Any
         :optional true}
  :Instance {:type :Kernel/Any
             :optional true}
  :QueryBy {:type :Kernel/Keyword
            :optional true}
  :QueryValue {:type :Kernel/String
               :optional true}})

(def ^:private list-attrs
  {:Record :Kernel/Path
   :Source :Kernel/Any
   :Fields {:check fields?}
   :View {:type :Kernel/Any
          :optional true}})

(entity :Fractl.UI/Table list-attrs)
(entity :Fractl.UI/Dashboard list-attrs)

(dataflow
 :Fractl.UI/RenderGenericInputForm
 {:Fractl.UI/InputForm
  {:Record :Fractl.UI/RenderGenericInputForm.Record
   :Fields :Fractl.UI/RenderGenericInputForm.Fields}})

(dataflow
 :Fractl.UI/RenderGenericInstanceForm
 {:Fractl.UI/InstanceForm
  {:Record :Fractl.UI/RenderGenericInstanceForm.Record
   :Fields :Fractl.UI/RenderGenericInstanceForm.Fields
   :Instance :Fractl.UI/RenderGenericInstanceForm.Instance
   :QueryBy :Fractl.UI/RenderGenericInstanceForm.QueryBy
   :QueryValue :Fractl.UI/RenderGenericInstanceForm.QueryValue}})

(dataflow
 :Fractl.UI/RenderGenericTable
 {:Fractl.UI/Table
  {:Record :Fractl.UI/RenderGenericTable.Record
   :Source :Fractl.UI/RenderGenericTable.Source
   :Fields :Fractl.UI/RenderGenericTable.Fields}})

(dataflow
 :Fractl.UI/RenderGenericDashboard
 {:Fractl.UI/Dashboard
  {:Record :Fractl.UI/RenderGenericDashboard.Record
   :Source :Fractl.UI/RenderGenericDashboard.Source
   :Fields :Fractl.UI/RenderGenericDashboard.Fields}})
