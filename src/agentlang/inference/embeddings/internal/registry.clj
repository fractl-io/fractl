(ns agentlang.inference.embeddings.internal.registry
  (:require [agentlang.global-state :as gs]))

(def ^:private registry (atom {}))

(defn register-db [db-name connector]
  (swap! registry assoc db-name connector)
  db-name)

(defn make-db-fn [db-name]
  (get @registry db-name))

(defn get-db
  ([]
   (get-db
    (:publish-schema (gs/get-app-config))))
  ([cfg]
   (when-let [f (make-db-fn (:vectordb cfg))]
     (f (:config cfg)))))
