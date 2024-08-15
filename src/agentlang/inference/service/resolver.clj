(ns agentlang.inference.service.resolver
  (:require [clojure.pprint :as pp]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.component :as cn]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry :as rg]
            [agentlang.lang.internal :as li]
            [agentlang.inference.service.logic :as logic]))

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
  (let [ents [:Agentlang.Inference.Service/Document]]
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
