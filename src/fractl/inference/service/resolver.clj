(ns fractl.inference.service.resolver
  (:require [clojure.pprint :as pp]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.lang.internal :as li]
            [fractl.inference.service.logic :as logic]))

(def ^:private create-handlers
  {:DocChunk (partial logic/handle-doc-chunk :add)
   :PlannerTool (partial logic/handle-planner-tool :add)
   :Question (partial logic/handle-app-question :add)})

(def ^:private delete-handlers
  {:PlannerTool (partial logic/handle-planner-tool :delete)})

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
(def ^:private resolver-delete (partial resolver-crud :delete delete-handlers))

(defn register-resolver []
  (let [ents (seq (cn/entity-names :Fractl.Inference.Service))]
    (rg/register-resolver-type
      :inference
      (fn [_ _]
        (r/make-resolver
          :inference
          {:create resolver-create
           :delete resolver-delete})))
    (rg/register-resolver
     {:name :inference
       :type :inference
       :paths ents})))
