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
         :optional true}
  :QueryBy {:type :Kernel/Keyword
            :optional true}
  :QueryValue {:type :Kernel/String
               :optional true}})

(entity
 :Fractl.UI/Table
 {:Record :Kernel/Path
  :Source :Kernel/Any
  :Fields {:check fields?}
  :View {:type :Kernel/Any
         :optional true}})

(entity
 :Fractl.UI/Dashboard
 {:Record :Kernel/Path
  :View {:type :Kernel/Any
         :optional true}})

(dataflow
 :Fractl.UI/RenderGenericInputForm
 {:Fractl.UI/InputForm
  {:Record :Fractl.UI/RenderGenericInputForm.RecordName
   :Fields :Fractl.UI/RenderGenericInputForm.Fields}})

(dataflow
 :Fractl.UI/RenderGenericDisplayForm
 {:Fractl.UI/InputForm
  {:Record :Fractl.UI/RenderGenericDisplayForm.RecordName
   :Fields :Fractl.UI/RenderGenericDisplayForm.Fields
   :QueryBy :Fractl.UI/RenderGenericDisplayForm.QueryBy
   :QueryValue :Fractl.UI/RenderGenericDisplayForm.QueryValue}})

(dataflow
 :Fractl.UI/RenderGenericTable
 {:Fractl.UI/Table
  {:Record :Fractl.UI/RenderGenericTable.RecordName
   :Source :Fractl.UI/RenderGenericTable.Source
   :Fields :Fractl.UI/RenderGenericTable.Fields}})
