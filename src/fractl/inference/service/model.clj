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
 {:Id {:type :UUID :default u/uuid-string :guid true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :Agent {:type :String :optional true}
  :Title :String
  :Content :Any})

(defn- agent-messages? [xs]
  (if (seq xs)
    (every? #(and (map? %)
                  (= 2 (count (keys %)))
                  (some #{(:role %)} #{:system :user :assistant})
                  (string? (:content %)))
            xs)
    true))

(defn- tool-components-list? [xs]
  (and (vector? xs) (every? li/name? xs)))

(entity
 :Fractl.Inference.Service/Agent
 {:Name {:type :String :guid true}
  :Type :String
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :ChatUuid {:type :UUID :default u/uuid-string}
  :UserInstruction {:type :String :optional true}
  :ToolComponents {:check tool-components-list? :optional true}
  :PromptFn {:check fn? :optional true}
  :Extension {:type :Map :optional true}
  :Context {:type :Map :optional true}
  :ResponseHandler {:check fn? :optional true}
  :Response {:type :Any :read-only true}})

(relationship
 :Fractl.Inference.Service/AgentDelegate
 {:meta {:between [:Fractl.Inference.Service/Agent
                   :Fractl.Inference.Service/Agent
                   :as [:Delegator :Delegatee]]}
  :Preprocessor {:type :Boolean :default false}})

(defn concat-results [rs]
  (vec (apply concat rs)))

(dataflow
 :Fractl.Inference.Service/FindAgentDelegates
 {:Fractl.Inference.Service/AgentDelegate
  {:Delegator? :Fractl.Inference.Service/FindAgentDelegates.Agent
   :Preprocessor? :Fractl.Inference.Service/FindAgentDelegates.Preprocessor}
  :as :Delegates}
 [:for-each :Delegates
  {:Fractl.Inference.Service/Agent
   {:Name? :%.Delegatee}}
  :as :Rs]
 [:eval '(fractl.inference.service.model/concat-results :Rs)])

(entity
 :Fractl.Inference.Service/ChatSession
 {:Id {:type :UUID :id true :default u/uuid-string}
  :Messages {:check agent-messages?}})

(relationship
 :Fractl.Inference.Service/AgentChatSession
 {:meta {:contains [:Fractl.Inference.Service/Agent :Fractl.Inference.Service/ChatSession]}})

(dataflow
 :Fractl.Inference.Service/LookupAgentChatSessions
 {:Fractl.Inference.Service/ChatSession? {}
  :-> [[:Fractl.Inference.Service/AgentChatSession?
        :Fractl.Inference.Service/LookupAgentChatSessions.Agent]]})

(dataflow
 :Fractl.Inference.Service/UpdateAgentChatSession
 {:Fractl.Inference.Service/ChatSession
  {li/id-attr? :Fractl.Inference.Service/UpdateAgentChatSession.SessionId
   :Messages :Fractl.Inference.Service/UpdateAgentChatSession.Messages}})

(record
 :Fractl.Inference.Service/ToolParam
 {:name :String
  :type :Any
  :required {:type :Boolean :default true}})

(defn- tool-param? [xs]
  (every? #(cn/make-instance :Fractl.Inference.Service/ToolParam %) xs))

(entity
 :Fractl.Inference.Service/Tool
 {:name {:type :String :guid true}
  :description :String
  :returns {:type :Keyword :optional true}
  :returns_many {:type :Boolean :default false}
  :params {:check tool-param?}
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
 :Fractl.Inference.Service/HasAgentDocChunks
 {:Fractl.Inference.Service/AgentDocChunk
  {:Agent? :Fractl.Inference.Service/HasAgentDocChunks.Agent}})

(dataflow
 :Fractl.Inference.Service/AgentDocChunks
 {:Fractl.Inference.Service/AgentDocChunk
  {:Agent? :Fractl.Inference.Service/AgentDocChunks.Agent} :as :R}
 [:for-each :R
  {:Fractl.Inference.Service/DocChunk
   {:Id? :%.DocChunk}}])

(defn- eval-event
  ([event callback]
   (when-let [result (first (e/eval-all-dataflows event))]
     (when (= :ok (:status result))
       (callback (:result result)))))
  ([event] (eval-event event identity)))

(defn- find-agent-delegates [preproc agent-instance]
  (eval-event
   {:Fractl.Inference.Service/FindAgentDelegates
    {:Agent (:Name agent-instance)
     :Preprocessor preproc}}))

(def find-agent-pre-delegates (partial find-agent-delegates true))
(def find-agent-post-delegates (partial find-agent-delegates false))

(defn- lookup-for-agent [event-name proc agent-instance]
  (eval-event
   {event-name
    {:Agent (:Name agent-instance)}}
   #(mapv proc %)))

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
    (str (:Title docchunk) " " (:Content docchunk))))

(def lookup-agent-docs (partial lookup-for-agent :Fractl.Inference.Service/AgentDocChunks normalize-docchunk))
(def has-agent-docs? (partial lookup-for-agent :Fractl.Inference.Service/HasAgentDocChunks seq))

(defn lookup-agent-chat-session [agent-instance]
  (eval-event
   {:Fractl.Inference.Service/LookupAgentChatSessions
    {:Agent agent-instance}}
   first))

(defn update-agent-chat-session [chat-session messages]
  (eval-event
   {:Fractl.Inference.Service/UpdateAgentChatSession
    {:SessionId (li/id-attr chat-session)
     :Messages messages}}
   identity))
