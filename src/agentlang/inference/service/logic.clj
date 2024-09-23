(ns agentlang.inference.service.logic
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.datafmt.json :as json]
            [agentlang.global-state :as gs]
            [agentlang.evaluator :as e]
            [agentlang.inference.provider :as provider]
            [agentlang.inference.provider.core :as p]
            [agentlang.inference.embeddings.core :as ec]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.service.tools :as tools]
            [agentlang.inference.service.lib.agent :as agent]
            [agentlang.inference.service.lib.prompt :as prompt])
  (:import (clojure.lang ExceptionInfo)))

(def ^:private generic-agent-handler (atom nil))

(defn set-generic-agent-handler! [f]
  (reset! generic-agent-handler f))

(defn handle-doc-chunk [operation instance]
  (when (= :add operation)
    (let [doc-chunk (cn/instance-attributes instance)
          app-uuid (:AppUuid doc-chunk)
          doc-name (:Title doc-chunk)
          chunk-text (:Content doc-chunk)]
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

(defn- log-trigger-agent! [instance]
  (log/info (str "Triggering " (:Type instance) " agent - " (u/pretty-str instance))))

(defn handle-planner-agent-deperecated [instance]
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [app-uuid (:AppUuid instance)
          question (:UserInstruction instance)
          qcontext (:Context instance)
          agent-config {:is-planner? true
                        :tools (model/lookup-agent-tools instance)
                        :docs "" ; TODO: lookup agent docs
                        :make-prompt (when-let [pfn (model/agent-prompt-fn instance)]
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
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [question (:UserInstruction instance)
          qcontext (:Context instance)
          ext (verify-analyzer-extension (:Extension instance))
          out-type (:OutputEntityType ext)
          out-scm (cn/ensure-schema out-type)
          pfn (model/agent-prompt-fn instance)
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
  (log/debug (str "### " (:Name agent-instance) "\n\n" result))
  (if-let [response
           (cond
             (string? result) result
             (map? result) (first (:Response result))
             (vector? result) (first result))]
    response
    result))

(def ^:private agent-prefix "agent:")
(def ^:private agent-prefix-len (count agent-prefix))

(defn- agent-filter-response [s]
  (when-let [idx (s/index-of s agent-prefix)]
    (s/trim (subs s (+ idx agent-prefix-len)))))

(defn- respond-with-agent [agent-name agents user-instruction]
  (if-let [agent (first (filter #(= agent-name (:Name %)) agents))]
    (:Response (@generic-agent-handler (assoc agent :UserInstruction user-instruction)))
    [(str "No delegate with name " agent-name) nil]))

(defn- compose-agents [agent-instance result]
  (if (vector? result)
    (let [[response model-info] result
          delegates (model/find-agent-post-delegates agent-instance)
          ins (:UserInstruction agent-instance)]
      (log/debug (str "Response from agent " (:Name agent-instance) " - " response))
      (if-let [agent-name (agent-filter-response response)]
        (respond-with-agent agent-name delegates (or (get-in agent-instance [:Context :UserInstruction]) ins))
        (if (seq delegates)
          (let [n (:Name agent-instance)
                rs (mapv #(let [ins (str (or (:UserInstruction %) "") "\n" response)]
                            (format-as-agent-response % (@generic-agent-handler (assoc % :UserInstruction ins))))
                         delegates)]
            [(apply str rs) model-info])
          result)))
    result))

(defn- update-delegate-user-instruction [delegate agent-instance]
  (if (= "ocr" (:Type delegate))
    (assoc delegate :Context (:Context agent-instance))
    (assoc delegate
           :Context (:Context agent-instance)
           :UserInstruction (str (or (:UserInstruction delegate) "")
                                 "\n"
                                 (:UserInstruction agent-instance)))))

(defn- call-preprocess-agents [agent-instance]
  (when-let [delegates (seq (model/find-agent-pre-delegates agent-instance))]
    (let [d (first delegates)
          [response model-info]
          (:Response (@generic-agent-handler (update-delegate-user-instruction d agent-instance)))]
      (log/debug (str "Response from pre-processor agent " (:Name d) "using llm " model-info " - " response))
      response)))

(defn- maybe-add-docs [docs user-ins]
  (if (seq docs)
    (str user-ins "\n Make use of the following knowledge-base:\n" (json/encode docs))
    user-ins))

(def ^:private agent-documents-limit 20)

(defn- maybe-lookup-agent-docs [agent-instance]
  (when (model/has-agent-docs? agent-instance)
    (let [embedding (provider/get-embedding {:text-content
                                             (json/encode {:Agent (:Name agent-instance)
                                                           :Content (:UserInstruction agent-instance)})})]
      (ec/find-similar-objects
       {:classname (ec/get-document-classname (:AppUuid agent-instance))
        :embedding embedding}
       agent-documents-limit))))

(defn handle-chat-agent [instance]
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [ins (:UserInstruction instance)
          docs (maybe-lookup-agent-docs instance)
          preprocessed-instruction (call-preprocess-agents instance)
          final-instruction (maybe-add-docs docs (or preprocessed-instruction ins))
          instance (assoc instance :UserInstruction final-instruction)]
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

(defn- maybe-add-tool-params [tool-instance]
  (let [f ((keyword (:type tool-instance)) tool-instance)]
    (if-not (:parameters f)
      (let [n (keyword (:name f))]
        (if (cn/entity? n)
          (tools/entity-to-tool n)
          (tools/event-to-tool n)))
      tool-instance)))

(defn- format-planner-result [r]
  (cond
    (or (vector? r) (map? r) (string? r)) r
    (seqable? r) (vec r)
    :else r))

(defn handle-planner-agent [instance]
  (log-trigger-agent! instance)
  (let [tools (vec (concat
                    (apply concat (mapv tools/all-tools-for-component (:ToolComponents instance)))
                    (mapv maybe-add-tool-params (model/lookup-agent-tools instance))))
        has-tools (seq tools)
        [result model-name] (handle-chat-agent
                             (if has-tools
                               (assoc instance :tools tools)
                               instance))
        _ (log/debug (str "Planner " (:Name instance) " raw result: " result))
        patterns (if has-tools
                   (mapv tools/tool-call-to-pattern result)
                   (if (string? result)
                     (read-string result)
                     result))]
    [(format-planner-result (u/safe-ok-result (e/eval-patterns :Agentlang.Core patterns))) model-name]))

(defn handle-ocr-agent [instance]
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(provider/make-ocr-completion instance)))
