(component :Selfservice.Core)

(entity
 :Ticket
 {:Id {:type :Int :guid true}
  :Title :String
  :Content {:type :String :optional true}})

(require '[agentlang.util :as u])
(require '[agentlang.util.logger :as log])
(require '[agentlang.util.http :as http])
(require '[agentlang.datafmt.json :as json])

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

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (u/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

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
       "`[{\"org\": \"acme-dev\", \"email\": \"kate@acme.com\"}]`. If the payload does not contain a ticket \n"
       "for github user addition, simply return an empty array, i.e `[]`. Do not return any other text.\n"
       "Now try to analyze the following payload:\n")}}

(inference :InvokeSelfService {:agent "self-service-agent"})

(defn as-json [result]
  (json/encode (mapv agentlang.component/instance-attributes result)))

(dataflow
 :ProcessTickets
 {:Ticket? {} :as :Result}
 [:eval '(user/as-json :Result) :as :S]
 {:InvokeSelfService {:UserInstruction :S}})
