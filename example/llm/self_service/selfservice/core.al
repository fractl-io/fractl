(component :Selfservice.Core)

(require '[agentlang.util :as u])
(require '[agentlang.util.logger :as log])
(require '[agentlang.util.http :as http])
(require '[agentlang.datafmt.json :as json])

(entity
 :Ticket
 {:Id {:type :Int :guid true}
  :Title :String
  :Content {:type :String :optional true}})

(entity
 :GithubMember
 {:Org :String
  :Email :Email})

(defn- extract-contents [content]
  (cond
    (vector? content)
    (apply #(if (seq %) (str "\n" %) "") (mapv extract-contents content))

    (map? content)
    (if-let [cnts (:content content)]
      (extract-contents cnts)
      (or (:text content) ""))
    
    :else ""))

(defn- lookup-ticket [url basic-auth id]
  (let [{status :status body :body}
        (http/do-get (str url id)
         {:headers {"Authorization" basic-auth "Accept" "application/json"}})]
    (if (= 200 status)  
      (let [obj (json/decode body)
            fields (:fields obj)]
        (agentlang.component/make-instance
         :Selfservice.Core/Ticket
         {:Id id :Title (:summary fields) :Content (extract-contents (:content (:description fields)))}))
      (log/warn (str "failed to lookup ticket " id ", status: " status)))))

(defn- fetch-individual-tickets [url basic-auth picker-response]
  (when-let [secs (seq (:sections picker-response))]
    (when-let [ids (seq (apply concat (mapv (fn [sec] (mapv :id (:issues sec))) secs)))]
      (vec (agentlang.util.seq/nonils (mapv (partial lookup-ticket url basic-auth) ids))))))

(defn ticket-query [connection _]
  (let [basic-auth (str "Basic " (agentlang.lang.b64/encode-string (str (:user connection) ":" (:token connection))))
        url (str (:root-url connection) "/rest/api/3/issue/")
        {status :status body :body}
        (http/do-get (str url "picker")
         {:headers {"Authorization" basic-auth "Accept" "application/json"}})]
    (if (= 200 status)
      (fetch-individual-tickets url basic-auth (json/decode body))
      (log/warn (str "lookup tickets failed with status: " status)))))

(resolver
 :TicketResolver
 {:with-methods
  {:query (partial ticket-query {:root-url (u/getenv "TICKETS_ROOT_URL")
                                 :user (u/getenv "TICKETS_USER")
                                 :token (u/getenv "TICKETS_TOKEN")})}
  :paths [:Selfservice.Core/Ticket]})

(defn- github-member-post [api-token inst]
  (let [result (http/do-post
                (str "https://api.github.com/orgs/" (:Org inst) "/invitations")
                {:headers
                 {"Accept" "application/vnd.github+json"
                  "Authorization" (str "Bearer " api-token)
                  "X-GitHub-Api-Version" "2022-11-28"}}
                {:email (:Email inst) :role "direct_member"})
        status (:status result)]
    (if (<= 200 status 300)
      inst
      (u/throw-ex (str "failed to add user " (:Email inst)
                       " to github org " (:Org inst)
                       ", with status " status " and reason " (:body result))))))

(resolver
 :GithubResolver
 {:with-methods
  {:create (partial github-member-post (u/getenv "GITHUB_API_TOKEN"))}
  :paths [:Selfservice.Core/GithubMember]})

(event :SubmitForApproval {:text :String})

(dataflow
 :SubmitForApproval
 {:Selfservice.Slack/Chat {:text :SubmitForApproval.text} :as :Chat}
 [:await
  {:Selfservice.Slack/Approval
   {:thread? :Chat.thread
    :channel? :Chat.channel}
   :as :Approval}
  :ok {:Selfservice.Core/GithubMember
       {:Org :Approval.data.Org
        :Email :Approval.data.Email}}])

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
  :Tools [{:name "Selfservice.Core/SubmitForApproval"}]
  :UserInstruction (str "You are an agent who use tools to create entity instances from json objects. For example, \n"
                        "you'll convert the array of json objects `[{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}]` \n"
                        "to the instances `[{:Selfservice.Core/SubmitForApproval {:text \"Please *approve* or *reject* the Github membership request `{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}`\"}}]`. (Make sure you do not include this sample in your response).\n"
                        "Now try to convert the following objects:")
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
       "`[{\"Org\": \"acme-dev\", \"Email\": \"kate@acme.com\"}]`. If the payload does not contain a ticket \n"
       "for github user addition, simply return an empty array, i.e `[]`. Do not return any other text.\n"
       "Now try to analyze the following payload:")
  :Delegates {:To "planner-agent"}}}

(inference :InvokeSelfService {:agent "self-service-agent"})

(defn as-json [result]
  (json/encode (mapv agentlang.component/instance-attributes result)))

(dataflow
 :ProcessTickets
 {:Ticket? {} :as :Result}
 [:eval '(user/as-json :Result) :as :S]
 {:InvokeSelfService {:UserInstruction :S}})
