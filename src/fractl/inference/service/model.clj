(ns fractl.inference.service.model
  (:require [clojure.string :as s]
            [fractl.lang :refer [component
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

(def ^:private doc-scheme-handlers {"file" slurp})
(def ^:private doc-schems (keys doc-scheme-handlers))
(def ^:private scheme-suffix "://")

(defn- document-resource-scheme [s]
  (when-let [idx (s/index-of s scheme-suffix)]
    (subs s 0 idx)))

(defn- document-uri? [s]
  (and (string? s)
       (when-let [scm (document-resource-scheme s)]
         (some #{s} doc-schemes))))

(defn- document-resource-name [s]
  (when-let [idx (s/index-of s scheme-suffix)]
    (subs s (+ idx 3))))

(defn read-doument-resource [uri]
  (when-let [h (get doc-scheme-handlers (document-resource-scheme uri))]
    (h (document-resource-name uri))))

(entity
 :Fractl.Inference.Service/Document
 {:Id {:type :UUID :default u/uuid-string :guid true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :Agent {:type :String :optional true}
  :Uri {:check document-uri?}
  :Title :String
  :Content '(fractl.inference.service.model/read-document-resource :Uri)})

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
                   :as [:From :To]]}
  :Preprocessor {:type :Boolean :default false}})

(defn concat-results [rs]
  (vec (apply concat rs)))

(dataflow
 :Fractl.Inference.Service/FindAgentDelegates
 {:Fractl.Inference.Service/AgentDelegate
  {:From? :Fractl.Inference.Service/FindAgentDelegates.Agent
   :Preprocessor? :Fractl.Inference.Service/FindAgentDelegates.Preprocessor}
  :as :Delegates}
 [:for-each :Delegates
  {:Fractl.Inference.Service/Agent
   {:Name? :%.Delegatee}}
  :as :Rs]
 [:eval '(fractl.inference.service.model/concat-results :Rs)])

(entity
 :Fractl.Inference.Service/ChatSession
 {:Id {:type :UUID :guid true :default u/uuid-string}
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
 :Fractl.Inference.Service/ResetAgentChatSessions
 [:eval '(fractl.inference.service.model/reset-agent-chat-session
          :Fractl.Inference.Service/ResetAgentChatSessions.Agent)])

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
 :Fractl.Inference.Service/AgentDocument
 {:meta {:between [:Fractl.Inference.Service/Agent :Fractl.Inference.Service/Document]}})

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
 :Fractl.Inference.Service/HasAgentDocuments
 {:Fractl.Inference.Service/AgentDocument
  {:Agent? :Fractl.Inference.Service/HasAgentDocuments.Agent}})

(dataflow
 :Fractl.Inference.Service/AgentDocuments
 {:Fractl.Inference.Service/AgentDocument
  {:Agent? :Fractl.Inference.Service/AgentDocuments.Agent} :as :R}
 [:for-each :R
  {:Fractl.Inference.Service/Document
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

(def lookup-agent-docs (partial lookup-for-agent :Fractl.Inference.Service/AgentDocuments normalize-docchunk))
(def has-agent-docs? (partial lookup-for-agent :Fractl.Inference.Service/HasAgentDocuments seq))

(defn lookup-agent-chat-session [agent-instance]
  (eval-event
   {:Fractl.Inference.Service/LookupAgentChatSessions
    {:Agent agent-instance}}
   first))

(defn update-agent-chat-session [chat-session messages]
  (eval-event
   {:Fractl.Inference.Service/Update_ChatSession
    {li/path-attr (li/path-attr chat-session)
     :Data {:Messages messages}}}
   identity true))

(defn reset-agent-chat-session [agent]
  (if (string? agent)
    (when-let [agent-instance (eval-event
                      {:Fractl.Inference.Service/Lookup_Agent
                       {:Name agent}}
                      first)]
      (reset-agent-chat-session agent-instance))
    (when-let [sess (lookup-agent-chat-session agent)]
      (let [msgs (vec (filter #(= :system (:role %)) (:Messages sess)))]
        (update-agent-chat-session sess msgs)))))
