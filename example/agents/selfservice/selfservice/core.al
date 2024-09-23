(component
 :Selfservice.Core
 {:clj-import '[(:require [agentlang.util :as u]
                          [agentlang.datafmt.json :as json]
                          [slack.core :as sc])
                (:use [slack.model] [ticket.model])]})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (u/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name "planner-agent"
  :Type "planner"
  :UserInstruction (str sc/agent-instructions "\n" "Now try to convert the following input to a dataflow:")
  :LLM "llm01"}}

{:Agentlang.Core/Agent
 {:Name "self-service-agent"
  :Type "chat"
  :LLM "llm01"
  :UserInstruction
  (str "You are an agent that identifies a self-service ticket for adding a user to a github organization. "
       "Tickets will be passed to you as a JSON payload, an example of which is "
       "`" (json/encode [{:Id 101, :Title "Laptop request", :Content "Please issue a laptop for json@acme.com"}
                         {:Id 102, :Title "Please add me to the github org",
                          :Content "Please add me (kate@acme.com) to the acme-dev organization."}])
       "`. Analyze the tickets and return the github org and the email of the user as JSON. "
       "For instance, with the above payload you should return: "
       "`[{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\", \"Id\": 102}]`. If the payload does not contain a ticket \n"
       "for github user addition, simply return an empty array, i.e `[]`. Do not return any other text.\n"
       "Now try to analyze the following payload:")
  :Delegates {:To "planner-agent"}}}

(inference :InvokeSelfService {:agent "self-service-agent"})

(dataflow
 :ProcessTickets
 {:Ticket.Core/Ticket? {} :as :Result}
 [:eval '(ticket.core/as-json :Result) :as :S]
 {:InvokeSelfService {:UserInstruction :S}})
