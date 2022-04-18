(ns fractl.resolver.ui.model
  (:require [fractl.lang
             :refer [component entity]]))

(component :Fractl.UI)

(entity
 :Fractl.UI/InputForm
 {:Record :Kernel/Path
  :Fields {:listof :Kernel/String}
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
  :Fields {:listof :Kernel/String}
  :View {:type :Kernel/Any
         :optional true}})
