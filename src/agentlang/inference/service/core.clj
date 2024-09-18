(ns agentlang.inference.service.core
  (:require [agentlang.util :as u]
            [agentlang.evaluator :as ev]
            [agentlang.global-state :as gs]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.service.resolver :as api-resolver]
            [agentlang.inference.provider.core :as p]))

(defn init [] (api-resolver/register-resolver))

(defn- setup-agent-document [agent-name doc-num doc-url]
  (let [doc-title (str agent-name "-doc-" doc-num)]
    (model/add-agent-document agent-name doc-title doc-url)))

(defn setup-agent-documents []
  (doseq [[agent-name docs] (:agent-documents (gs/get-app-config))]
    (mapv (fn [doc i] (setup-agent-document agent-name (inc i) doc)) docs (range (count docs))))
  true)
