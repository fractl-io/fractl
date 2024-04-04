(ns
 fractl.model.model
 (:require
  [fractl.model.fractl.kernel.lang :as fractl.kernel.lang]
  [fractl.model.fractl.kernel.identity :as fractl.kernel.identity]
  [fractl.model.fractl.kernel.rbac :as fractl.kernel.rbac]
  [fractl.model.fractl.llm.core :as fractl.llm.core]))
(fractl.lang/model
 {:name :Fractl,
  :fractl-version "current",
  :components
  [:Fractl.Kernel.Lang
   :Fractl.Kernel.Identity
   :Fractl.Kernel.Rbac
   :Fractl.Llm.Core]})
(def fractl___MODEL_ID__ "08038746-3d3d-44f6-8e6b-11e0e12b3ed9")
