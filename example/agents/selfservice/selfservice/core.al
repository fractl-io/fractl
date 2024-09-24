(component
 :Selfservice.Core
 {:refer [:Slack.Core :Ticket.Core]
  :clj-import (quote [(:require [agentlang.util :as u]
                                [agentlang.datafmt.json :as json])])})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (u/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name "request-classifier"
  :Type "chat"
  :LLM "llm01"
  :UserInstruction (str "Classify the input text to one of the categories - approve or reject. "
                        "For example if the input is `you can join the team`, your response must be `approve`. "
                        "If the input is `sorry, can't allow`, your response must be `reject`.\n"
                        "If you are unable to classify the text, simply return `reject`.\n"
                        "(Do not include the ticks (`) in your response).\n"
                        "Now please classify the following text following these rules.\n")}}

(event
 :InvokeClassifier
 {:UserInstruction :String})

(inference :InvokeClassifier {:agent "request-classifier"})

(def planner-instructions
  (str "Convert an array of json objects that represent request to join Github organizations into a dataflow which will first "
       "raise an approval request through a slack-channel and only if the request is approved the user is added to the "
       "Github organization. An example input is \n"
       "`[{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}]` and it represents a sequence of user-requests to join "
       "a Github organization. These requests should be pushed to a slack channel as chat messages. \n"
       "Each chat message must have its text set as the following example: \n"
       "\"Please *approve* or *reject* the Github membership request `{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}` \n"
       "The format must be closely followed. Once the message is in the slack channel, a manager will review the request and "
       "respond with a message and this will be in the :response attribute of the slack-chat. To generate the final dataflow, \n"
       "You have to,\n"
       "  - invoke the classifier agent with the manager response as the user-instruction."
       "  - If the classifier says `approve`, then \n"
       "  ---- create a ticket-comment with the body \"approved\"\n"
       "  ---- create a github-member with the appropriate org and email values.\n"
       "  - else \n"
       "  ---- create a ticket-comment with body \"rejected\"\n\n"
       "The ticket-id for the ticket-comment can be accessed from the `data.Id` field of the slack-chat instance.\n"
       "The org and email for the github-member will be available as the chat's `data.Org` and `data.Email` fields. \n"
       "You must not create a github-member for a rejected request.\n"))

{:Agentlang.Core/Agent
 {:Name "planner-agent"
  :Type "planner"
  :Tools (u/as-agent-tools [:Slack.Core/Chat :Ticket.Core/Ticket :Ticket.Core/TicketComment :Ticket.Core/GithubMember
                            :Selfservice.Core/InvokeClassifier])
  :UserInstruction (str planner-instructions "\n" "Now try to convert the following input to a dataflow:")
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
 [:eval (quote (ticket.core/as-json :Result)) :as :S]
 {:InvokeSelfService {:UserInstruction :S}})

(dataflow
 :ProcessWebhook
 [:eval (quote (ticket.core/as-json :ProcessWebhook.Tickets)) :as :S]
 {:InvokeSelfService {:UserInstruction :S}})
