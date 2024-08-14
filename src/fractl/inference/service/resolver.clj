(ns fractl.inference.service.resolver
  (:require [clojure.pprint :as pp]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.lang.internal :as li]
            [fractl.inference.service.logic :as logic]))

(def ^:private create-handlers
  {:Document (partial logic/handle-doc-chunk :add)})

(defn- get-handler [handlers instance]
  (let [[_ n] (li/split-path (cn/instance-type instance))]
    (n handlers)))

(defn- resolver-crud [operation handlers instance]
  (if-let [handler (get-handler handlers instance)]
    (try
      (handler instance)
      (catch Exception ex
        (log/exception ex)))
    (log/warn (str "Cannot " (name operation) " " (cn/instance-type instance)))))

(def ^:private resolver-create (partial resolver-crud :create create-handlers))

(defn register-resolver []
  (let [ents [:Fractl.Inference.Service/Document]]
    (rg/register-resolver-type
     :inference
      (fn [_ _]
        (r/make-resolver
         :inference
         {:create resolver-create})))
    (rg/register-resolver
     {:name :inference
      :type :inference
      :compose? true
      :paths ents})))
