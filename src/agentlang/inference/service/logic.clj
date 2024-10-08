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

(defn- start-chat [agent-instance]
  ;; TODO: integrate messaging resolver
  (println (str (:Name agent-instance) ": " (:UserInstruction agent-instance)))
  (:ChatUuid agent-instance))

(defn- get-next-chat-message [_]
  ;; TODO: integrate messaging resolver
  (print " ? ")
  (flush)
  (read-line))

(defn- make-chat-completion [instance]
  (let [agent-name (:Name instance)
        chat-id (start-chat instance)]
    (loop [iter 0, instance instance]
      (if (< iter 5)
        (let [[result _ :as r] (provider/make-completion instance)
              chat-session (model/lookup-agent-chat-session instance)
              msgs (:Messages chat-session)]
          (log/debug (str "Response " iter " from " agent-name " - " result))
          (if (= \{ (first (s/trim result)))
            (do (println (str agent-name ": Thanks, your request is queued for processing."))
                r)
            (do (println (str agent-name ": " result))
                (model/update-agent-chat-session
                 chat-session
                 (vec (concat msgs [{:role :user :content (get-next-chat-message chat-id)}])))
                (recur (inc iter) (if (zero? iter) (dissoc instance :UserInstruction) instance)))))
        (do (println (str agent-name ": session expired"))
            [(json/encode {:error "chat session with agent " agent-name " has expired."}) "agentlang"])))))

(defn handle-orchestrator-agent [instance]
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [ins (:UserInstruction instance)
          docs (maybe-lookup-agent-docs instance)
          preprocessed-instruction (call-preprocess-agents instance)
          final-instruction (maybe-add-docs docs (or preprocessed-instruction ins))
          instance (assoc instance :UserInstruction final-instruction)]
      (compose-agents instance (make-chat-completion instance)))))

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

(def ^:private generic-planner-instructions
  (str "Consider the following entity and event definitions:\n"
       (u/pretty-str
        '(entity
          :Acme.Core/Customer
          {:Email {:type :Email :guid true}
           :Name :String
           :Address {:type :String :optional true}
           :LoyaltyPoints {:type :Int :default 50}}))
       "\n\n"
       (u/pretty-str
        '(event
          :Acme.Core/LookupCustomerByEmail
          {:Email :Email}))
       "\n\n"
       (u/pretty-str
        '(entity
          :Acme.Core/PlatinumCustomer
          {:Email :Email}))
       "\n\n"
       (u/pretty-str
        '(entity
          :Acme.Core/GoldenCustomer
          {:Email :Email}))
       "\n\nIf the instruction given to you is to construct a customer instance with name `joe` and email `joe@acme.com`,\n"
       "you must return the pattern:\n"
       (u/pretty-str
        [{:Acme.Core/Customer {:Email "joe@acme.com" :Name "joe"} :as :Customer}])
       "\nThere's no need to fill in attributes marked `:optional true`, `:read-only true` or those with a `:default` value, unless explicitly instructed.\n"
       "You can also ignore attributes with types `:Now` and `:Identity` - these will be automatically filled-in by the system.\n"
       "For example, if the instruction is to create customer `joe` with email `joe@acme.com` and loyalty points 6700, then you must return\n"
       (u/pretty-str
        [{:Acme.Core/Customer {:Email "joe@acme.com" :Name "joe", :LoyaltyPoints 6700} :as :Customer}])
       "\nIf the instruction is to lookup a customer, then you can create an event pattern:\n"
       (u/pretty-str
        [{:Acme.Core/LookupCustomerByEmail {:Email "joe@acme.com"} :as :Customer}])
       "\nYou can also generate patterns that are evaluated against conditions, using the `:match` clause. For example,\n"
       "if the instruction is to create a customer named `joe` with email `joe@acme.com` and then apply the following \n"
       "business rules:\n"
       "1. If the loyalty-points is 50, return the customer instance.\n"
       "2. If the loyalty-points is greater than 50 and less than 1000, mark the customer as golden.\n"
       "3. Otherwise, mark the customer as platinum\n"
       "Given the above instruction, you must return the following dataflow patterns:\n"
       (u/pretty-str
        [{:Acme.Core/Customer {:Name "joe" :Email "joe@acme.com"} :as :Customer}
         [:match
          [:= :Customer.LoyaltyPoints 50] [:Customer]
          [:and [:> :Customer.LoyaltyPoints 50] [:< :Customer.LoyaltyPoints 1000]]
          [{:Acme.Core/GoldenCustomer {:Email :Customer.Email}}]
          [{:Acme.Core/PlatinumCustomer {:Email :Customer.Email}}]]])
       "\n\nConsider the syntax of the `:match` pattern - there are conditions and consequences. `[:= :Customer.LoyaltyPoints 50]` "
       "is an example of a condition. `[:Customer]` is its consequence. There is also an `else` part for `:match`, which is a "
       "pattern that will be evaluated if all conditions return false. In this example "
       "`[{:Acme.Core/PlatinumCustomer {:Email :Customer.Email}}]` is the else-pattern.\n"
       ;; "In addition to entities, you may also create patterns to invoke AI agents. Such invocations will look like:\n"
       ;; (u/pretty-str
       ;;  {:Acme.Core/InvokeAnAgent
       ;;   {:UserInstruction "hello, there"}
       ;;   :as :ResponseFromAgent})
       ;; "\n\nResponse from an agent is usually some text and can be handled in a `:match` patterns as:\n"
       ;; (u/pretty-str
       ;;  [:match :ResponseFromAgent
       ;;   "hi" "happy"
       ;;   "hello" "happy"
       ;;   "sad"])
       ;; "\n\nThat was a simple example on invoking ai agents from dataflow patterns.\n"
       "Now that you understand how to translate business workflows (or dataflows) into entity and `:match` patterns, "
       "consider the entity definitions and user-instructions that follows to generate fresh dataflow patterns. "
       "An important note: do not return any plain text in your response, only return the vector of dataflow patterns.\n"
       "If your input contains patterns of instances like `{:Acme.Core/Employee {:Name \"sam\" :Salary 1000 :Email \"sam@came.com\"} :as :E1}`, keep those "
       "at the top of the generated dataflow so that reference to `:E1` can be used later. An example of such usage is: \n"
       (u/pretty-str
        {:Acme.Core/EmailMessage
         {:Email :E1.Email
          :Message '(str "hello " :E1.Name ", welcome aboard!")}
         :as :AnEmailMessage})
       ;; "\nIf the user instruction contains an `:Input` instance, please add that also to the top of the dataflow."
       "\nNote the function call expression is preceded by a single-quote and references uses a simple dot-notation. "
       "There's no parenthesis needed for references.\n"
       "A note about aliases - aliases are names defiled as `:as :SomeName` and attached to a pattern. "
       "If you create a reference like `:Customer.Email`, make sure the alias `:Customer` is attached to a previous instance pattern. "
       "Always attach aliases to instance patterns outside of a `:match`. But never attach aliases to instance patterns inside a `:match`."
       "\nAnother important thing you should keep in mind: your response must not include any objects from the previous "
       "examples. Your response should only make use of the entities and other definitions provided by the user below.\n\n"))

(defn- agent-tools-as-definitions [instance]
  (str
   (when-let [cns (:ToolComponents instance)]
     (tools/raw-components cns))
   (tools/as-raw-tools
    (mapv (fn [tool]
            (let [f ((keyword (:type tool)) tool)]
              (keyword (:name f))))
          (model/lookup-agent-tools instance)))))

(defn- instance-results? [xs]
  (every? cn/an-instance? xs))

(defn- dataflow-patterns? [xs]
  (if (vector? xs)
    (if (instance-results? xs)
      false
      true)
    false))

(defn handle-planner-agent [instance]
  (log-trigger-agent! instance)
  (let [instance (assoc instance :UserInstruction
                        (str generic-planner-instructions
                             "Entity definitions from user:\n\n" (agent-tools-as-definitions instance)
                             "Instruction from user:\n\n" (:UserInstruction instance)))
        _ (log/debug (str "Updated instruction for agent " (:Name instance) ": " (:UserInstruction instance)))
        tools [] #_(vec (concat
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
    (if (seq patterns)
      (do (log/debug (str "Patterns generated by " (:Name instance) ": "
                          (u/pretty-str patterns)))
          [(format-planner-result
            (if (dataflow-patterns? patterns)
              (u/safe-ok-result (e/eval-patterns :Agentlang.Core patterns))
              patterns))
           model-name])
      [{:result :noop} model-name])))

(defn handle-ocr-agent [instance]
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(provider/make-ocr-completion instance)))
