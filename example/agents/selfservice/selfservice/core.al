(component
 :Selfservice.Core
 {:refer [:Slack.Core :Ticket.Core]})

(record
 :Request
 {:Org :String
  :Email :Email
  :Id :String})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"}}

{:Agentlang.Core/Agent
 {:Name "workflow-agent"
  :Type "planner"
  :Tools [{:name "Selfservice.Core/Request"}
          {:name "Slack.Core/Chat"}
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
  :Type "planner"
  :LLM "llm01"
  :Tools [{:name "Selfservice.Core/Request"}]
  :UserInstruction
  "You are an agent that identifies a self-service ticket for adding a user to a github organization.
Tickets will be passed to you as a JSON payload. Analyze the tickets and return instances of Request with the
github org, email and ticket id as attributes."
  :Delegates {:To "workflow-agent"}}}

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

{:Agentlang.Core/Agent
 {:Name "tickets-agent"
  :Type "planner"
  :LLM "llm01"
  :Tools [{:name "Ticket.Core/Ticket"}]
  :UserInstruction
  "You are an agent that converts a sequence of requests to join a github organization to tickets.
The content of the ticket must contain both the github org and the user's email."}}

{:Agentlang.Core/Agent
 {:Name "orchestrator-agent"
  :Type "orchestrator"
  :Chat {:Messages [{:role :system
                     :content (str "You are an agent who help users provision github accounts. "
                                   "For this a user has to provide his/her email address and the name of the "
                                   "Github organization.\nOnce you have all the details, return the organization and email in the format:\n"
                                   "{\"Organization\": <user-provided-github-org-name-here>, \"Email\": <user's-email-here>}.\n"
                                   "Do not add any other text to your final response.")}]}
  :LLM "llm01"
  :Delegates {:To "tickets-agent"}}}

(inference :InvokeOrchestrator {:agent "orchestrator-agent"})
