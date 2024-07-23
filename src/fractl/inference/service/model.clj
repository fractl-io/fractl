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
 {:Id :Identity
  :AppUuid {:type :UUID :default u/uuid-string}
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

(entity
 :Fractl.Inference.Service/Agent
 {:Name {:type :String :guid true}
  :Type {:type :String :unique true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :ChatUuid {:type :UUID :default u/uuid-string}
  :UserInstruction {:type :String :optional true}
  :PromptFn {:check fn? :optional true}
  :Extension {:type :Map :optional true}
  :Context {:type :Map :optional true}
  :Response {:type :Any :read-only true}})

(record
 :Fractl.Inference.Service/ToolParam
 {:name :String
  :type :Any
  :required {:type :Boolean :default true}})

(entity
 :Fractl.Inference.Service/Tool
 {:name {:type :String :guid true}
  :description :String
  :returns {:type :Keyword :optional true}
  :returns_many {:type :Boolean :default false}
  :params {:listof :Fractl.Inference.Service/ToolParam}
  :df_patterns :Edn})

(relationship
 :Fractl.Inference.Service/AgentLLM
 {:meta {:between [:Fractl.Inference.Service/Agent :Fractl.Inference.Provider/LLM]}})

(relationship
 :Fractl.Inference.Service/AgentTool
 {:meta {:between [:Fractl.Inference.Service/Agent :Fractl.Inference.Service/Tool]}})

(relationship
 :Fractl.Inference.Service/AgentDocChunk
 {:meta {:between [:Fractl.Inference.Service/Agent :Fractl.Inference.Service/DocChunk]}})

(dataflow
 :Fractl.Inference.Service/LLMsForAgent
 {:Fractl.Inference.Service/AgentLLM
  {:Agent? :Fractl.Inference.Service/LLMsForAgent.Agent}})

(dataflow
 :Fractl.Inference.Service/AgentTools
 {:Fractl.Inference.Service/AgentTool
  {:Agent? :Fractl.Inference.Service/AgentTools.Agent} :as :R}
 [:for-each :R
  {:Fractl.Inference.Service/Tool
   {:name? :%.Tool}}])

(dataflow
 :Fractl.Inference.Service/AgentDocChunks
 {:Fractl.Inference.Service/AgentDocChunk
  {:Agent? :Fractl.Inference.Service/AgentDocChunks.Agent} :as :R}
 [:for-each :R
  {:Fractl.Inference.Service/DocChunk
   {:Id? :%.DocChunk}}])

(defn- lookup-for-agent [event-name proc agent-instance]
  (when-let [result (first (e/eval-all-dataflows
                            {event-name
                             {:Agent (:Name agent-instance)}}))]
    (when (= :ok (:status result))
      (mapv proc (:result result)))))

(def lookup-llms-for-agent (partial lookup-for-agent :Fractl.Inference.Service/LLMsForAgent :LLM))

(defn ensure-llm-for-agent [agent-instance]
  (if-let [llm (first (lookup-llms-for-agent agent-instance))]
    llm
    (u/throw-ex (str "No LLM attached to agent " (:Name agent-instance)))))

(defn- normalize-tool [tool]
  (let [tool (if (map? tool) tool (first tool))
        attrs (cn/instance-attributes tool)]
    (dissoc
     (assoc
      attrs
      :returns-many (:returns_many attrs)
      :df-patterns (:df_patterns attrs))
     :returns_many :df_patterns)))

(def lookup-agent-tools (partial lookup-for-agent :Fractl.Inference.Service/AgentTools normalize-tool))

(defn- normalize-docchunk [docchunk]
  (let [docchunk (if (map? docchunk) docchunk (first docchunk))]
    (str (:DocName docchunk) " " (:DocChunk docchunk))))

(def lookup-agent-docs (partial lookup-for-agent :Fractl.Inference.Service/AgentDocChunks normalize-docchunk))
