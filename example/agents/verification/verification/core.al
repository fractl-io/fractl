(component :Verification.Core)

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

;; Demonstrates agent-composition, where the answer of one agent is verified by another.
{:Agentlang.Core/Agent
 {:Name "verification-agent"
  :Type "chat"
  :LLM "llm01"
  :Chat
  {:Messages
   [{:role :system
     :content (str "You are an agent who verifies the answer returned by another agent. "
                   "Analyse the chain-of-thought returned by the other agent and return YES "
                   "if its conlusion is correct. Otherwise return NO. The final answer will be "
                   "encoded by the other agent as - ANSWER is: <some-text>")}]}}}
{:Agentlang.Core/Agent
 {:Name "chain-of-thought-agent"
  :Type "chat"
  :LLM "llm01"
  :Chat
  {:Messages
   [{:role :system
     :content (str "You are an agent who answer user queries by taking advantage of "
                   "a chain-of-thought. That means, you will take a step-by-step approach "
                   "in your response, cite sources and give reasoning before sharing final answer "
                   "in the below format: ANSWER is: <name>")}]}
  :Delegates {:To "verification-agent"}}}

;; Usage:
;; POST api/Verification.Core/AnswerWithVerification
;; {"Verification.Core/AnswerWithVerification":
;;  {"UserInstruction": "Who was the most decorated (maximum medals) individual athlete in the Olympic games that were held at Sydney?"}}
(inference :AnswerWithVerification {:agent "chain-of-thought-agent"})
