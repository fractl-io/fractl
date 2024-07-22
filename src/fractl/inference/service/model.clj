(ns fractl.inference.service.model
  (:require [fractl.lang :refer [component
                                 dataflow
                                 entity
                                 event
                                 record
                                 relationship]]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.evaluator :as e]
            [fractl.lang.internal :as li]
            [fractl.inference.provider.model]))

(component :Fractl.Inference.Service)

(entity
 :Fractl.Inference.Service/DocChunk
 {:AppUuid {:type :UUID :default u/uuid-string}
  :DocName :String
  :DocChunk :Any})

(defn tool-spec-param-type? [t]
  (and (map? t)
       (string? (:type t))
       (if-let [f (:format t)]
         (string? f)
         true)))

(defn tool-spec-param? [p]
  (and (map? p)
       (string? (:name p))
       (tool-spec-param-type? (:type p))
       (boolean? (:required p))))

(defn tool-spec? [obj]
  (and (map? obj)
       (string? (:description obj))
       (every? tool-spec-param? (:params obj))
       (vector? (:df-patterns obj))))

(entity
 :Fractl.Inference.Service/PlannerTool
 {:Id {:type :String :guid true :read-only true :default u/uuid-string}
  :AppUuid {:type :UUID :default u/uuid-string}
  :ToolName {:type :String :optional true}
  :ToolSpec {:check tool-spec?}
  :Tag :String
  :Type :String
  :MetaContent :String})

(record
 :Fractl.Inference.Service/QuestionOptions
 {:UseDocs {:type :Boolean :default true}
  :UseTools {:type :Boolean :default true}
  ;; tools related options (applicable if :UseTools is true)
  :Classification {:type :Boolean :default true}
  :ChainOfThought {:type :Boolean :default true}})

(entity
 :Fractl.Inference.Service/Question
 {:ChatUuid {:type :UUID :default u/uuid-string}
  :AppUuid :UUID
  :AgentConfig {:type :Map :optional true}
  :Question :String
  :QuestionContext {:type :Map :default {}}
  :QuestionOptions {:type :Map :default {}}
  :QuestionResponse {:type :Any :optional true :read-only true}})

(record
 :Fractl.Inference.Service/Agent
 {:Name {:type :String :guid true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :ChatUuid {:type :UUID :default u/uuid-string}
  :Messages {:listof :String :optional true}
  :UserInstruction {:type :String :optional true}
  :PromptFn {:check fn? :optional true}
  :Context {:type :Map :optional true}
  :Response {:type :Any :read-only true}})

(entity
 :Fractl.Inference.Service/AnalysisAgent
 {:meta {:inherits :Fractl.Inference.Service/Agent}
  :Comment :String
  :OutputEntityType :Keyword
  :OutputAttributes {:listof :Keyword :optional true}
  :OutputAttributeValues {:type :String :optional true}})

(relationship
 :Fractl.Inference.Service/AnalysisAgentLLM
 {:meta {:between [:Fractl.Inference.Service/AnalysisAgent :Fractl.Inference.Provider/LLM]}})

(entity
 :Fractl.Inference.Service/PlannerAgent
 {:meta {:inherits :Fractl.Inference.Service/Agent}
  :Tools {:listof :Map :optional true}
  :Docs {:listof :String :optional true}})

(relationship
 :Fractl.Inference.Service/PlannerAgentLLM
 {:meta {:between [:Fractl.Inference.Service/PlannerAgent :Fractl.Inference.Provider/LLM]}})

(defn lookup-llms-for-agent [agent-instance]
  (let [[c n] (li/split-path (cn/instance-type agent-instance))
        rel-name (li/make-path c (keyword (str (name n) "LLM")))]
    (when-let [llms (e/safe-eval-pattern {rel-name {(li/name-as-query-pattern n) (:Name agent-instance)}})]
      (map :LLM llms))))

(defn ensure-llm-for-agent [agent-instance]
  (if-let [llm (seq (lookup-llms-for-agent agent-instance))]
    llm
    (u/throw-ex (str "No LLM attached to agent " (:Name agent-instance)))))
