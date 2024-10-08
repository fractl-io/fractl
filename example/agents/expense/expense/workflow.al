(component :Expense.Workflow)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name "receipt-ocr-agent"
  :Type "ocr"
  :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                        "No need to include sub-totals, totals and other data.")
  :LLM "llm01"}}

{:Agentlang.Core/Agent
 {:Name "expense-agent"
  :Type "eval"
  :LLM "llm01"
  :Chat
  {:Messages
   [:q# [{:role :system
          :content (str "You are an intelligent agent who summarizes an expense report as a list of expense instances. For example, "
                        "if the report is \"I spent $200 on air ticket and $80 on food\", the summary should be "
                        "[{:Expense.Workflow/Expense {:Title \"air ticket\" :Amount 200}}, "
                        "{:Expense.Workflow/Expense {:Title \"food\", :Amount 80}}]")}]]}
  :Delegates {:To "receipt-ocr-agent"
              :Preprocessor true}}}

;; Usage:
;; POST api/Expense.Workflow/SaveExpenses
;; {"Expense.Workflow/SaveExpenses": {"UserInstruction": "https://acme.com/bill/myexpense.jpg"}}
(inference :SaveExpenses {:agent "expense-agent"})
