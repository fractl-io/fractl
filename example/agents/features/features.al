(component :Features)

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name "cot-agent"
  :Type "chat"
  :LLM "llm01"
  :UserInstruction "You are an agent who counsels people on their life-problems"
  :Features ["chain-of-thought"]}}

{:Agentlang.Core/Agent
 {:Name "cot-sc-agent"
  :Type "chat"
  :LLM "llm01"
  :UserInstruction "You are an agent who counsels people on their life-problems"
  :Features ["chain-of-thought" "self-critique"]}}

(inference :Chat01 {:agent "cot-agent"})
(inference :Chat02 {:agent "cot-sc-agent"})
