(ns fractl.inference.service.model
  (:require [fractl.lang :refer [component
                                 dataflow
                                 entity
                                 event
                                 record
                                 relationship]]
            [fractl.component :as cn]
            [fractl.util :as u]))

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

(defn llm-spec? [x]
  (or (string? x) ;; llm-instance name
      (cn/instance-of? :Fractl.Inference.Provider/LLM x)))

(record
 :Fractl.Inference.Service/Agent
 {:Name {:type :String :guid true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :ChatUuid {:type :UUID :default u/uuid-string}
  :LLM {:check llm-spec?}
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

(entity
 :Fractl.Inference.Service/PlannerAgent
 {:meta {:inherits :Fractl.Inference.Service/Agent}
  :Tools {:listof :Map :optional true}
  :Docs {:listof :String :optional true}})
