(ns fractl.inference.service.logic
  (:require [clojure.edn :as edn]
            [fractl.component :as fc]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.global-state :as gs]
            [fractl.inference.embeddings.internal.registry :as r]
            [fractl.inference.embeddings.protocol :as p]
            [fractl.inference.service.lib.agent :as agent]
            [fractl.inference.service.lib.prompt :as prompt])
  (:import (clojure.lang ExceptionInfo)))

(defn- get-vectdb-connection []
  (let [config (gs/get-app-config)]
    (if-let [connector (r/fetch-db-connector (:vectordb config))]
      (connector (:config config))
      (u/throw-ex (str "Unsupported embbeddings database type: " (:vectordb config))))))

(defn post-doc-chunk [instance]
  (let [doc-chunk (fc/instance-attributes instance)
        app-uuid (:AppUuid doc-chunk)
        doc-name (:DocName doc-chunk)
        chunk-text (:DocChunk doc-chunk)]
    (log/debug (u/pretty-str "Ingesting doc chunk" doc-chunk))
    (p/embed-document-chunk (get-vectdb-connection) app-uuid doc-chunk)
    instance))

(defn post-planner-tool [instance]
  (let [planner-tool (fc/instance-attributes instance)
        app-uuid (:AppUuid planner-tool)
        tool-name (:ToolName planner-tool)
        tool-spec (when-let [tspec (:ToolSpec planner-tool)]
                    (-> tspec
                        (update :df-patterns edn/read-string)))
        tag (:Tag planner-tool)
        type (:Type planner-tool)
        meta-content (:MetaContent planner-tool)
        operation (:Operation planner-tool)
        db-conn (get-vectdb-connection)]
    (log/debug (u/pretty-str "Ingesting planner tool" planner-tool))
    (if (or (and (nil? tool-name)
                 (nil? tool-spec))
            (= tag 'component))
      (log/info (u/pretty-str "Ignoring insertion of component for now..."))
      (case operation
        :add (p/update-tool db-conn {:app-uuid app-uuid
                                     :tool-spec (-> tool-spec
                                                    (assoc :tool-name tool-name))
                                     :meta-content meta-content
                                     :tag tag
                                     :type type})
        :delete (p/delete-tool db-conn {:app-uuid app-uuid :tag tag :type type})
        (throw (ex-info "Expected operation :add or :delete" {:operation operation}))))
    instance))

(defn answer-question [app-uuid question-text qcontext {:keys [use-docs?
                                                               use-schema?]
                                                        :as options}]
  (let [agent-args {:user-question question-text
                    :background qcontext
                    :use-docs? use-docs?
                    :app-uuid app-uuid}]
    (try
      (if use-schema?
        (-> (agent/make-planner-agent agent-args)
            (apply [agent-args])
            (select-keys [:answer-text
                          :patterns
                          :errormsg]))
        (-> (agent/make-docs-rag-agent agent-args)
            (apply [agent-args])
            (select-keys [:answer-text])))
      (catch ExceptionInfo e
        (log/error e)
        {:errormsg (u/pretty-str (ex-message e) (ex-data e))})
      (catch Exception e
        (log/error e)
        {:errormsg (.getMessage e)}))))

(defn post-app-question [request-data]
  (let [app-uuid (:AppUuid request-data)
        question (:Question request-data)
        qcontext (:QuestionContext request-data)
        in-event (when-let [inference-event (first (:inference-event qcontext))]
                   (val inference-event))
        options {:use-schema? (get-in request-data [:QuestionOptions :UseSchema])
                 :use-docs?   (get-in request-data [:QuestionOptions :UseDocs])}
        response (answer-question app-uuid question (or qcontext {}) options)]
    (assoc request-data
      :QuestionContext {} ; empty :QuestionContext to avoid entity-name conflict
      :QuestionResponse (pr-str response))))
