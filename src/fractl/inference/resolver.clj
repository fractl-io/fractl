(ns fractl.inference.resolver
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(defn- llm-create [_ instance]
  instance)

(defn- llm-eval [_ event-instance]
  event-instance)

(defn- register-resolver [config entity-names]
  (let [k :fractl-inference]
    (rg/register-resolver-type
     k (fn [_ _]
         (r/make-resolver
          k {:create (partial llm-create config)
             :eval (partial llm-eval config)})))
    (rg/register-resolver {:name k :type k :paths entity-names})))

(defn register [config]
  (if-let [ents (seq (cn/entity-names :Fractl.Llm.Core))]
    (register-resolver config ents)
    (u/throw-ex "Fractl.Llm model not initialized.")))
