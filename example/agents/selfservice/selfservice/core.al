(component
 :Selfservice.Core
 {:refer [:Slack.Core :Ticket.Core]})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"}}

{:Agentlang.Core/Agent
 {:Name "planner-agent"
  :Type "planner"
  :Tools [{:name "Slack.Core/Chat"}
          {:name "Ticket.Core/Ticket"}
          {:name "Ticket.Core/TicketComment"}
          {:name "Ticket.Core/GithubMember"}]
  :UserInstruction "You'll receive some tickets with requests from users to join GitHub organizations. Follow the following steps:
1. For each user, send an approval request as a slack chat. This message must include the user's email, github org name and the ticket Id.
2. If the slack chat response is to approve the request, then
     a. update the ticket with the comment \"approved\".
     b. add the user as a member to the github org.
   If the response is not to approve, then update the ticket with the comment \"rejected\"."
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
