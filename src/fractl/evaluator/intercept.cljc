(ns fractl.evaluator.intercept
  (:require [fractl.util.logger :as log]
            [fractl.evaluator.intercept.rbac :as irbac]
            [fractl.evaluator.intercept.core :as interceptors]))

(def ^:private makers {:rbac irbac/make})

(defn- make-interceptor [n]
  (when-let [make (makers n)]
    (log/info (str "initializing interceptor - " (name n)))
    (make)))

(defn init-interceptors [interceptor-names]
  (mapv
   #(interceptors/add-interceptor!
     (make-interceptor %))
   interceptor-names))

(def reset-interceptors! interceptors/reset-interceptors!)
