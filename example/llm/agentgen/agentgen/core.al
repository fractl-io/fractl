(component :Agentgen.Core)

(require '[agentlang.datafmt.json :as json])
(require '[agentlang.inference.service.model :as model])

(def agent-name "agent-generator-agent")

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name agent-name
  :Type "chat"
  :LLM "llm01"
  :Chat
  {:Messages
   [:q# [{:role :system
          :content (str "You are an agent who analyse a user request to generate another agent. Let me call you AgentGenerator.\n"
                        "For example, if the user-request is to generate a simple chat agent, you should return:\n
                        {\"name\": \"a-chat-agent\", \"type\": \"instruction\", \"you are an agent that chats with a human user.\"}\n"
                        "The various types of agents that you can create are:\n"
                        "1. chat - simple text chat agents\n"
                        "2. ocr - agents that can extract text from images\n"
                        "3. eval - agents that can evaluate dataflow patterns\n"
                        "4. planner - an agent that can generate dataflow patterns using tools and text.\n\n"
                        "What follows are a few more examples of user requests and your responses:\n"
                        "User: I want an agent to analyse images of receipts and make summary reports.\n"
                        "AgentGenerator: {\"name\": \"receipt-image-analyser\", \"type\": \"ocr\", \"instruction\": \"Analyse the image of a receipt and return a summary report with items and totals.\"}"
                        "User: I want an agent to analyse images of receipts and make summary reports. Another agent will take the summary report and use the following entity to insert each expense item into the database: (entity :Acme/Expense {:Title :String :Amount :Double}). For example, if the summary report is \"I spent $200 on air ticket and $80 on food\", the response must be [{:Acme/Expense {:Title \"air ticket\" :Amount 200}}, {:Acme/Expense {:Title \"food\", :Amount 80}}]\n"
                        "AgentGenerator: [{\"name\": \"receipt-image-analyser\", \"type\": \"ocr\", \"instruction\": \"Analyse the image of a receipt and return a summary report with items and totals.\"}, {\"name\": \"expense-creator\", \"type\": \"eval\", \"delegate-of\": \"receipt-image-analyser\", \"instruction\": \"You are an intelligent agent who summarizes an expense report as a list of expense instances. For example, if the report is \"I spent $200 on air ticket and $80 on food\", the response must be [{:Acme/Expense {:Title \"air ticket\" :Amount 200}}, {:Acme/Expense {:Title \"food\", :Amount 80}}]")}]]}}}

(defn map-to-agent [obj]
  {:Agentlang.Core/Agent
   (merge
    {:Name (:name obj)
     :Type (:type obj)
     :UserInstruction (or (:instruction obj) "")
     :LLM "<<your-llm-spec-or-name>>"}
    (when-let [d (:delegate-to obj)]
      {:Delegates {:To d}})
    (when-let [message (:chat obj)]
      {:Chat message}))})

(defn- delegate-for [spec delegates]
  (let [n (:name spec)]
    (first (filter #(= n (:delegate-of %)) delegates))))

(defn- preproc-delegates [specs]
  (if-let [delegates (seq (filter :delegate-of specs))]
    (mapv (fn [spec]
            (if-let [delegate (delegate-for spec delegates)]
              (assoc spec :delegate-to (:name delegate))
              spec))
          specs)
    specs))

(defn generate-agent [spec]
  (cond
    (map? spec) (map-to-agent spec)
    (and (seqable? spec) (not (string? spec))) (mapv generate-agent (preproc-delegates spec))
    :else spec))

(model/set-agent-response-handler!
 agent-name
 (fn [s]
   [(pr-str (generate-agent (json/decode (first s)))) (second s)]))

(inference :GenerateAgent {:agent agent-name})
