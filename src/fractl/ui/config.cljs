(ns fractl.ui.config
  (:require [fractl.global-state :as gs]))

(defn dashboard
  ([config]
   (get-in config [:view :dashboard]))
  ([] (dashboard (gs/get-app-config))))

(defn component
  ([config]
   (get-in config [:view :component]))
  ([] (component (gs/get-app-config))))
