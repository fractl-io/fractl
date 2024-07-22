(ns fractl.inference.service.agent-registry
  (:require [fractl.inference.service.logic :as logic]))

(def ^:private agent-registry (atom {}))

(defn register-agent-handler [agent-type handler]
  (swap! agent-registry assoc agent-type handler)
  agent-type)

(defn fetch-agent-handler [agent-type]
  (get @agent-registry agent-type))

(register-agent-handler "Planner" logic/handle-planner-agent)
(register-agent-handler "Analyzer" logic/handle-analysis-agent)
