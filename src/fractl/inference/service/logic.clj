(ns fractl.inference.service.logic
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [fractl.component :as fc]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.global-state :as gs]
            [fractl.inference.embeddings.core :as ec]
            [fractl.inference.service.lib.agent :as agent]
            [fractl.inference.service.lib.prompt :as prompt])
  (:import (clojure.lang ExceptionInfo)))

(defn handle-doc-chunk [operation instance]
  (when (= :add operation)
    (let [doc-chunk (fc/instance-attributes instance)
          app-uuid (:AppUuid doc-chunk)
          doc-name (:DocName doc-chunk)
          chunk-text (:DocChunk doc-chunk)]
      (log/debug (u/pretty-str "Ingesting doc chunk" doc-chunk))
      (ec/embed-document-chunk app-uuid doc-chunk)
      instance)))

(defn- assoc-tool-id [instance]
  (str (:AppUuid instance) "__"
       (:Tag instance) "__"
       (:Type instance)))

(defn- parse-tool-id [instance]
  (let [[app-uuid tag type] (s/split (:Id instance) #"__")]
    {:app-uuid app-uuid :tag tag :type type}))

(defn handle-planner-tool [operation instance]
  (let [planner-tool (fc/instance-attributes instance)
        app-uuid (:AppUuid planner-tool)
        tool-name (:ToolName planner-tool)
        tool-spec (when-let [tspec (:ToolSpec planner-tool)]
                    (-> tspec
                        (update :df-patterns edn/read-string)))
        tag (:Tag planner-tool)
        type (:Type planner-tool)
        meta-content (:MetaContent planner-tool)]
    (log/debug (u/pretty-str "Ingesting planner tool" planner-tool))
    (if (or (and (nil? tool-name)
                 (nil? tool-spec))
            (= tag 'component))
      (log/info (u/pretty-str "Ignoring insertion of component for now..."))
      (case operation
        :add (do (ec/update-tool {:app-uuid app-uuid
                                  :tool-spec (-> tool-spec
                                                 (assoc :tool-name tool-name))
                                  :meta-content meta-content
                                  :tag tag
                                  :type type})
                 (assoc-tool-id instance))
        :delete (let [args (parse-tool-id instance)]
                  (ec/delete-tool args))
        (throw (ex-info "Expected operation :add or :delete" {:operation operation}))))
    instance))

(defn answer-question [app-uuid question-text
                       qcontext {:keys [use-docs?
                                        use-schema?]
                                 :as options}
                       agent-config]
  (let [agent-args {:user-question question-text
                    :background qcontext
                    :use-docs? use-docs?
                    :app-uuid app-uuid
                    :agent-config (:config agent-config)}]
    (try
      (if use-schema?
        (-> (agent/make-planner-agent agent-args)
            (apply [(dissoc agent-args :agent-config)])
            (select-keys [:answer-text
                          :patterns
                          :errormsg]))
        (-> (agent/make-docs-rag-agent agent-args)
            (apply [(dissoc agent-args :agent-config)])
            (select-keys [:answer-text])))
      (catch ExceptionInfo e
        (log/error e)
        {:errormsg (u/pretty-str (ex-message e) (ex-data e))})
      (catch Exception e
        (log/error e)
        {:errormsg (.getMessage e)}))))

(defn answer-question-analyze [app-uuid question-text qcontext options]
  (let [agent-args (merge {:user-statement question-text
                           :payload qcontext}
                          (:config options))]
    (try
      (-> (agent/make-analyzer-agent agent-args)
          (apply [agent-args])
          (select-keys [:answer-text
                        :patterns
                        :errormsg]))
      (catch ExceptionInfo e
        (log/error e)
        {:errormsg (u/pretty-str (ex-message e) (ex-data e))})
      (catch Exception e
        (log/error e)
        {:errormsg (.getMessage e)}))))

(defn handle-app-question [operation instance]
  (if (= :add operation)
    (let [app-uuid (:AppUuid instance)
          question (:Question instance)
          qcontext (:QuestionContext instance)
          in-event (when-let [inference-event (first (:inference-event qcontext))]
                     (val inference-event))
          agent-config (:AgentConfig instance)
          options {:use-schema? (get-in instance [:QuestionOptions :UseSchema])
                   :use-docs? (get-in instance [:QuestionOptions :UseDocs])}
          response (if-not (:is-planner? (:config agent-config))
                     (answer-question-analyze app-uuid question (or qcontext {})
                                              (merge options agent-config))
                     (answer-question app-uuid question (or qcontext {}) options agent-config))]
      (assoc instance
             :QuestionContext {} ; empty :QuestionContext to avoid entity-name conflict
             :QuestionResponse (pr-str response)))
    instance))
