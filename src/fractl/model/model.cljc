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
(def fractl___MODEL_ID__ "81244b53-9638-49e9-95c5-fc8b87bc0811")
