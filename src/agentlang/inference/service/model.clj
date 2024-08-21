(ns agentlang.inference.service.model
  (:require [clojure.string :as s]
            [agentlang.lang :refer [component
                                    dataflow
                                    entity
                                    event
                                    record
                                    attribute
                                    relationship
                                    syntax]]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.evaluator :as e]
            [agentlang.lang.internal :as li]
            [agentlang.inference.provider.model]))

(component :Agentlang.Inference.Service)

(def ^:private doc-scheme-handlers {"file" slurp})
(def ^:private doc-schemes (keys doc-scheme-handlers))
(def ^:private scheme-suffix "://")

(defn- document-resource-scheme [s]
  (when-let [idx (s/index-of s scheme-suffix)]
    (subs s 0 idx)))

(defn- document-uri? [s]
  (and (string? s)
       (when-let [scm (document-resource-scheme s)]
         (some #{scm} doc-schemes))))

(defn- document-resource-name [s]
  (when-let [idx (s/index-of s scheme-suffix)]
    (subs s (+ idx 3))))

(defn read-document-resource [uri]
  (when-let [h (get doc-scheme-handlers (document-resource-scheme uri))]
    (h (document-resource-name uri))))

(entity
 :Agentlang.Inference.Service/Document
 {:Id {:type :UUID :default u/uuid-string :guid true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :Agent {:type :String :optional true}
  :Uri {:check document-uri?}
  :Title :String
  :Content '(agentlang.inference.service.model/read-document-resource :Uri)})

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
 :Agentlang.Inference.Service/Agent
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
  :Response {:type :Any :read-only true}
  :CacheChatSession {:type :Boolean :default true}})

(relationship
 :Agentlang.Inference.Service/AgentDelegate
 {:meta {:between [:Agentlang.Inference.Service/Agent
                   :Agentlang.Inference.Service/Agent
                   :as [:From :To]]}
  :Preprocessor {:type :Boolean :default false}})

(attribute
 :Agentlang.Inference.Service/Delegates
 {:extend :Agentlang.Inference.Service/Agent
  :type :Agentlang.Inference.Service/AgentDelegate
  :relationship :Agentlang.Inference.Service/AgentDelegate})

(defn concat-results [rs] (vec (apply concat rs)))

(dataflow
 :Agentlang.Inference.Service/FindAgentDelegates
 {:Agentlang.Inference.Service/AgentDelegate
  {:From? :Agentlang.Inference.Service/FindAgentDelegates.Agent
   :Preprocessor? :Agentlang.Inference.Service/FindAgentDelegates.Preprocessor}
  :as :Delegates}
 [:for-each :Delegates
  {:Agentlang.Inference.Service/Agent
   {:Name? :%.To}}
  :as :Rs]
 [:eval '(agentlang.inference.service.model/concat-results :Rs)])

(entity
 :Agentlang.Inference.Service/ChatSession
 {:Id {:type :UUID :guid true :default u/uuid-string}
  :Messages {:check agent-messages?}})

(relationship
 :Agentlang.Inference.Service/AgentChatSession
 {:meta {:contains [:Agentlang.Inference.Service/Agent :Agentlang.Inference.Service/ChatSession]}})

(attribute
 :Agentlang.Inference.Service/Chat
 {:extend :Agentlang.Inference.Service/Agent
  :type :Agentlang.Inference.Service/ChatSession
  :relationship :Agentlang.Inference.Service/AgentChatSession})

(dataflow
 :Agentlang.Inference.Service/LookupAgentChatSessions
 {:Agentlang.Inference.Service/ChatSession? {}
  :-> [[:Agentlang.Inference.Service/AgentChatSession?
        :Agentlang.Inference.Service/LookupAgentChatSessions.Agent]]})

(dataflow
 :Agentlang.Inference.Service/ResetAgentChatSessions
 [:eval '(agentlang.inference.service.model/reset-agent-chat-session
          :Agentlang.Inference.Service/ResetAgentChatSessions.Agent)])

(record
 :Agentlang.Inference.Service/ToolParam
 {:name :String
  :type :Any
  :required {:type :Boolean :default true}})

(defn- tool-param? [xs]
  (every? #(cn/make-instance :Agentlang.Inference.Service/ToolParam %) xs))

(entity
 :Agentlang.Inference.Service/Tool
 {:name {:type :String :guid true}
  :description :String
  :returns {:type :Keyword :optional true}
  :returns_many {:type :Boolean :default false}
  :params {:check tool-param?}
  :df_patterns :Edn})

(relationship
 :Agentlang.Inference.Service/AgentLLM
 {:meta {:between [:Agentlang.Inference.Service/Agent :Agentlang.Inference.Provider/LLM]}})

(attribute
 :Agentlang.Inference.Service/LLM
 {:extend :Agentlang.Inference.Service/Agent
  :type :Agentlang.Inference.Provider/LLM
  :relationship :Agentlang.Inference.Service/AgentLLM
  :order 0})

(relationship
 :Agentlang.Inference.Service/AgentTool
 {:meta {:between [:Agentlang.Inference.Service/Agent :Agentlang.Inference.Service/Tool]}})

(attribute
 :Agentlang.Inference.Service/Tools
 {:extend :Agentlang.Inference.Service/Agent
  :type :Agentlang.Inference.Service/Tool
  :relationship :Agentlang.Inference.Service/AgentTool})

(relationship
 :Agentlang.Inference.Service/AgentDocument
 {:meta {:between [:Agentlang.Inference.Service/Agent :Agentlang.Inference.Service/Document]}})

(attribute
 :Agentlang.Inference.Service/Documents
 {:extend :Agentlang.Inference.Service/Agent
  :type :Agentlang.Inference.Service/Document
  :relationship :Agentlang.Inference.Service/AgentDocument})

(dataflow
 :Agentlang.Inference.Service/LLMsForAgent
 {:Agentlang.Inference.Service/AgentLLM
  {:Agent? :Agentlang.Inference.Service/LLMsForAgent.Agent}})

(dataflow
 :Agentlang.Inference.Service/AgentTools
 {:Agentlang.Inference.Service/AgentTool
  {:Agent? :Agentlang.Inference.Service/AgentTools.Agent} :as :R}
 [:for-each :R
  {:Agentlang.Inference.Service/Tool
   {:name? :%.Tool}}])

(dataflow
 :Agentlang.Inference.Service/HasAgentDocuments
 {:Agentlang.Inference.Service/AgentDocument
  {:Agent? :Agentlang.Inference.Service/HasAgentDocuments.Agent}})

(dataflow
 :Agentlang.Inference.Service/AgentDocuments
 {:Agentlang.Inference.Service/AgentDocument
  {:Agent? :Agentlang.Inference.Service/AgentDocuments.Agent} :as :R}
 [:for-each :R
  {:Agentlang.Inference.Service/Document
   {:Id? :%.Document}}])

(defn- eval-event
  ([event callback atomic?]
   (when-let [result (first ((if atomic?
                               e/eval-all-dataflows-atomic
                               e/eval-all-dataflows)
                             event))]
     (when (= :ok (:status result))
       (callback (:result result)))))
  ([event callback] (eval-event event callback false))
  ([event] (eval-event event identity)))

(defn- find-agent-delegates [preproc agent-instance]
  (eval-event
   {:Agentlang.Inference.Service/FindAgentDelegates
    {:Agent (:Name agent-instance)
     :Preprocessor preproc}}))

(def find-agent-pre-delegates (partial find-agent-delegates true))
(def find-agent-post-delegates (partial find-agent-delegates false))

(defn- lookup-for-agent [event-name proc agent-instance]
  (eval-event
   {event-name
    {:Agent (:Name agent-instance)}}
   #(mapv proc %)))

(def lookup-llms-for-agent (partial lookup-for-agent :Agentlang.Inference.Service/LLMsForAgent :LLM))

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

(def lookup-agent-tools (partial lookup-for-agent :Agentlang.Inference.Service/AgentTools normalize-tool))

(defn- normalize-docchunk [docchunk]
  (let [docchunk (if (map? docchunk) docchunk (first docchunk))]
    (str (:Title docchunk) " " (:Content docchunk))))

(def lookup-agent-docs (partial lookup-for-agent :Agentlang.Inference.Service/AgentDocuments normalize-docchunk))
(def has-agent-docs? (partial lookup-for-agent :Agentlang.Inference.Service/HasAgentDocuments seq))

(defn lookup-agent-chat-session [agent-instance]
  (eval-event
   {:Agentlang.Inference.Service/LookupAgentChatSessions
    {:Agent agent-instance}}
   first))

(defn update-agent-chat-session [chat-session messages]
  (eval-event
   {:Agentlang.Inference.Service/Update_ChatSession
    {li/path-attr (li/path-attr chat-session)
     :Data {:Messages messages}}}
   identity true))

(defn reset-agent-chat-session [agent]
  (if (string? agent)
    (when-let [agent-instance (eval-event
                      {:Agentlang.Inference.Service/Lookup_Agent
                       {:Name agent}}
                      first)]
      (reset-agent-chat-session agent-instance))
    (when-let [sess (lookup-agent-chat-session agent)]
      (let [msgs (vec (filter #(= :system (:role %)) (:Messages sess)))]
        (update-agent-chat-session sess msgs)))))

(syntax agent :Agentlang.Inference.Service/Agent {:ident :Name})
