(ns
 fractl.model.model
 (:require
  [fractl.model.fractl.kernel.lang :as fractl.kernel.lang]
  [fractl.model.fractl.kernel.identity :as fractl.kernel.identity]
  [fractl.model.fractl.kernel.rbac :as fractl.kernel.rbac]))
(fractl.lang/model
 {:name :Fractl,
  :fractl-version "current",
  :components
  [:Fractl.Kernel.Lang :Fractl.Kernel.Identity :Fractl.Kernel.Rbac]})
(def fractl___MODEL_ID__ "53394138-b2da-4268-ad45-90e8263ed3cc")
