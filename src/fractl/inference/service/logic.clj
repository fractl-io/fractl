(ns fractl.inference.service.logic
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.global-state :as gs]
            [fractl.evaluator :as e]
            [fractl.inference.provider :as provider]
            [fractl.inference.provider.core :as p]
            [fractl.inference.embeddings.core :as ec]
            [fractl.inference.service.model :as model]
            [fractl.inference.service.lib.agent :as agent]
            [fractl.inference.service.lib.prompt :as prompt])
  (:import (clojure.lang ExceptionInfo)))

(def ^:private generic-agent-handler (atom nil))

(defn set-generic-agent-handler! [f]
  (reset! generic-agent-handler f))

(defn handle-doc-chunk [operation instance]
  (when (= :add operation)
    (let [doc-chunk (cn/instance-attributes instance)
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

(defn answer-question [app-uuid question-text
                       qcontext {:keys [use-docs?
                                        use-schema?]
                                 :as options}
                       agent-config]
  (let [agent-args {:user-question question-text
                    :background qcontext
                    :use-docs? use-docs?
                    :app-uuid app-uuid
                    :agent-config agent-config}]
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

(defn answer-question-analyze [question-text qcontext agent-config]
  (let [agent-args (merge {:user-statement question-text
                           :payload qcontext}
                          agent-config)]
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

(defn handle-planner-agent [instance]
  (log/info (str "Triggering planner agent - " (u/pretty-str instance)))
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [app-uuid (:AppUuid instance)
          question (str (:UserInstruction instance)
                        (or (get-in instance [:Context :UserInstruction]) ""))
          qcontext (:Context instance)
          agent-config {:is-planner? true
                        :tools (model/lookup-agent-tools instance)
                        :docs (model/lookup-agent-docs instance)
                        :make-prompt (when-let [pfn (:PromptFn instance)]
                                       (partial pfn instance))}
          options {:use-schema? true :use-docs? true}]
      (answer-question app-uuid question (or qcontext {}) options agent-config))))

(defn- verify-analyzer-extension [ext]
  (when ext
    (when-not (u/keys-in-set? ext #{:Comment :OutputEntityType
                                    :OutputAttributes :OutputAttributeValues})
      (u/throw-ex (str "Invalid keys in analyzer agent extension")))
    ext))

(defn handle-analysis-agent [instance]
  (log/info (str "Triggering analysis agent - " (u/pretty-str instance)))
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [question (:UserInstruction instance)
          qcontext (:Context instance)
          ext (verify-analyzer-extension (:Extension instance))
          out-type (:OutputEntityType ext)
          out-scm (cn/ensure-schema out-type)
          pfn (:PromptFn instance)
          agent-config
          (assoc
           (if pfn
             {:make-prompt (partial pfn instance)}
             {:information-type (:Comment ext)
              :output-keys (or (:OutputAttributes ext)
                               (vec (cn/user-attribute-names out-scm)))
              :output-key-values (or (:OutputAttributeValues ext)
                                     (cn/schema-as-string out-scm))})
           :result-entity out-type)]
      (answer-question-analyze question (or qcontext {}) agent-config))))

(defn- format-as-agent-response [agent-instance result]
  ;; TODO: response parsing should also move to agent-registry,
  ;; one handler will be needed for each type of agent.
  (if-let [response
           (cond
             (string? result) result
             (map? result) (first (:Response result))
             (vector? result) (first result))]
    (str "### " (:Name agent-instance) "\n\n" response)
    result))

(defn- compose-agents [agent-instance result]
  (if (vector? result)
    (let [[response model-info] result
          delegates (model/find-agent-post-delegates agent-instance)]
      (if (seq delegates)
        (let [n (:Name agent-instance)
              ins (str "Instruction for agent " n " was ### " (:UserInstruction agent-instance) " ### "
                       "The response from " n " is ### " response " ###")
              rs (mapv #(format-as-agent-response % (@generic-agent-handler (assoc % :UserInstruction ins))) delegates)]
          [(str (format-as-agent-response agent-instance response) "\n\n" (apply str rs)) model-info])
        result))
    result))

(defn- call-preprocess-agents [agent-instance]
  (when-let [delegates (seq (model/find-agent-pre-delegates agent-instance))]
    (let [d (first delegates)
          [response model-info]
          (:Response
           (@generic-agent-handler
            (assoc d :Context (:Context agent-instance))))]
      (log/debug (str "Response from pre-processor agent " (:Name d) "using llm " model-info " - " response))
      response)))

(defn handle-chat-agent [instance]
  (log/info (str "Triggering " (:Type instance) " agent - " (u/pretty-str instance)))
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [preprocessed-instruction (call-preprocess-agents instance)
          instance (if preprocessed-instruction
                     (assoc-in instance [:Context :UserInstruction] preprocessed-instruction)
                     instance)]
      (compose-agents instance (provider/make-completion instance)))))

(defn- maybe-eval-patterns [[response _]]
  (if (string? response)
    (if-let [pats
             (let [exp (read-string response)]
               (cond
                 (vector? exp) exp
                 (map? exp) [exp]))]
      (mapv e/safe-eval-pattern pats)
      response)
    response))

(defn handle-eval-agent [instance]
  (maybe-eval-patterns (handle-chat-agent instance)))

(defn handle-ocr-agent [instance]
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(provider/make-ocr-completion instance)))
