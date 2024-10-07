(component :Planner.Core)

(entity
 :Customer
 {:Email {:type :Email :guid true}
  :Name :String
  :Created :Now
  :Type {:oneof ["premium" "standard"]}})

(entity
 :Employee
 {:Email {:type :Email :guid true}
  :Name :String
  :Created :Now
  :Department {:oneof ["sales" "accounting"]}})

{:Agentlang.Core/LLM
 {:Type :openai
  :Name :llm01
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name :planner-agent
  :Type :planner
  ;;:ToolComponents ["Planner.Core"]
  :Tools [:Planner.Core/Customer :Planner.Core/Employee]
  :UserInstruction "You are an agent that use tools to create entity instances from text descritpions."
  :LLM :llm01}}

(event
 :InvokePlanner
 {:meta {:inherits :Agentlang.Core/Inference}})

{:Agentlang.Core/Agent
 {:Name :data-summary-agent
  :Type :chat
  :CacheChatSession false
  :LLM :llm01
  :Chat
  {:Messages
   [{:role :system
     :content (str "You are an agent who translates a text to a data-summary. For example, if the input text is "
                   "\"A new premium customer needs to be added to the system with email joe@acme.com and name Joe J\", "
                   "your response should be - \"Customer: type - premium, email - joe@acme.com, name - Joe J\"")}]}
  :Delegates {:To :planner-agent}
  :Input :InvokePlanner}}

;; Usage:
;; POST api/Planner.Core/InvokePlanner
;; {"Planner.Core/InvokePlanner":
;;   {"UserInstruction": "Add a new employee named Mat to the sales department. His email is mat@acme.com"}}

