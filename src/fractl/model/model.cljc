(ns
 fractl.model.model
 (:require
  [fractl.model.fractl.kernel.lang :as fractl.kernel.lang]
  [fractl.model.fractl.kernel.identity :as fractl.kernel.identity]
  [fractl.model.fractl.kernel.rbac :as fractl.kernel.rbac]
  [fractl.model.fractl.kernel.store :as fractl.kernel.store]))
(fractl.lang/model
 {:name :Fractl,
  :fractl-version "current",
  :components
  [:Fractl.Kernel.Lang
   :Fractl.Kernel.Identity
   :Fractl.Kernel.Rbac
   :Fractl.Kernel.Store]})
(def fractl___MODEL_ID__ "392c48c2-322b-47de-b89a-9cf280b1ca77")
