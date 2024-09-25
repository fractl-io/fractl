(component
 :Selfservice.Core
 {:refer [:Slack.Core :Ticket.Core]})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"}}

{:Agentlang.Core/Agent
 {:Name "request-classifier"
  :Type "chat"
  :LLM "llm01"
  :UserInstruction
  "Classify the input text to one of the categories - approve or reject.
For example if the input is `you can join the team`, your response must be `approve`.
If the input is `sorry, can't allow`, your response must be `reject`.
If you are unable to classify the text, simply return `reject`.
(Do not include the ticks (`) in your response).
Now please classify the following text following these rules.\n\n"}}

(event :InvokeClassifier {:UserInstruction :String})

(inference :InvokeClassifier {:agent "request-classifier"})

{:Agentlang.Core/Agent
 {:Name "planner-agent"
  :Type "planner"
  :Tools [{:name "Slack.Core/Chat"}
          {:name "Ticket.Core/Ticket"}
          {:name "Ticket.Core/TicketComment"}
          {:name "Ticket.Core/GithubMember"}
          {:name "Selfservice.Core/InvokeClassifier"}]
  :UserInstruction  "Convert an array of json objects that represent request to join Github organizations into
a dataflow which will first raise an approval request through a slack-channel and only if the request is approved
the user is added to the Github organization. An example input is
`[{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}]` and it represents a sequence of user-requests to join
a Github organization. These requests should be pushed to a slack channel as chat messages.
Each chat message must have its text set as the following example:
\"Please *approve* or *reject* the Github membership request `{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}`
The format must be closely followed. Once the message is in the slack channel, a manager will review the request and
respond with a message and this will be in the :response attribute of the slack-chat. To generate the final dataflow,
You have to,
 - invoke the classifier agent with the manager response as the user-instruction.
 - If the classifier says `approve`, then
 ---- create a ticket-comment with the body \"approved\"
 ---- create a github-member with the appropriate org and email values.
 - else
 ---- create a ticket-comment with body \"rejected\"
The ticket-id for the ticket-comment can be accessed from the `data.Id` field of the slack-chat instance.
The org and email for the github-member will be available as the chat's `data.Org` and `data.Email` fields.
You must not create a github-member for a rejected request.
Now try to convert the following input to a dataflow:\n\n"
  :LLM "llm01"}}

{:Agentlang.Core/Agent
 {:Name "self-service-agent"
  :Type "chat"
  :LLM "llm01"
  :UserInstruction
  "You are an agent that identifies a self-service ticket for adding a user to a github organization.
Tickets will be passed to you as a JSON payload, an example of which is
`[{\"Id\": 101, \"Title\": \"Laptop request\", \"Content\": \"Please issue a laptop for json@acme.com\"}
  {\"Id\": 102, \"Title\": \"Please add me to the github org\", \"Content\": \"Please add me (kate@acme.com) to the acme-dev organization.\"}]`
Analyze the tickets and return the github org and the email of the user as JSON.
For instance, with the above payload you should return:
`[{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\", \"Id\": 102}]`. If the payload does not contain a ticket
for github user addition, simply return an empty array, i.e `[]`. Do not return any other text.
Now try to analyze the following payload:\n"
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
