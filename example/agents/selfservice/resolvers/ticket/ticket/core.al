(component
 :Ticket.Core
 {:clj-import '[(:require [agentlang.util :as u]
                          [agentlang.util.logger :as log]
                          [agentlang.util.http :as http]
                          [agentlang.util.seq :as us]
                          [agentlang.datafmt.json :as json]
                          [agentlang.component :as cn]
                          [agentlang.lang.b64 :as b64])]})

(entity
 :Ticket
 {:Id {:type :Int :guid true}
  :Title :String
  :Content {:type :String :optional true}})

(entity
 :TicketComment
 {:TicketId :Int
  :Body :String})

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
        (cn/make-instance
         :Ticket.Core/Ticket
         {:Id id :Title (:summary fields) :Content (extract-contents (:content (:description fields)))}))
      (log/warn (str "failed to lookup ticket " id ", status: " status)))))

(defn- fetch-individual-tickets [url basic-auth picker-response]
  (when-let [secs (seq (:sections picker-response))]
    (when-let [ids (seq (apply concat (mapv (fn [sec] (mapv :id (:issues sec))) secs)))]
      (vec (us/nonils (mapv (partial lookup-ticket url basic-auth) ids))))))

(defn- ticket-basic-auth [connection]
  (str "Basic " (b64/encode-string (str (:user connection) ":" (:token connection)))))

(defn- make-headers [connection]
  (let [basic-auth (ticket-basic-auth connection)]
    {:headers {"Authorization" basic-auth "Accept" "application/json"}}))

(defn ticket-query [connection _]
  (let [url (str (:root-url connection) "/rest/api/3/issue/")
        headers (make-headers connection)
        {status :status body :body} (http/do-get (str url "picker") headers)
        basic-auth (get-in headers [:headers "Authorization"])]
    (if (= 200 status)
      (fetch-individual-tickets url basic-auth (json/decode body))
      (log/warn (str "lookup tickets failed with status: " status)))))

(defn- make-comment-body [text]
  {"content"
   [{"content" [{"text" text "type" "text"}]
     "type" "paragraph"}]
   "type" "doc"
   "version" 1})

(defn ticket-comment-create [connection instance]
  (let [url (str (:root-url connection) "/rest/api/3/issue/" (:TicketId instance) "/comment")
        headers (make-headers connection)
        body {:body (make-comment-body (:Body instance))}
        {status :status :as response} (http/do-post url headers body)]
    (if (or (= 201 status) (= 200 status))
      instance
      (log/warn (str "create ticket-comment returned status: " status)))))

(def tickets-connection-info
  {:root-url (u/getenv "TICKETS_ROOT_URL")
   :user (u/getenv "TICKETS_USER")
   :token (u/getenv "TICKETS_TOKEN")})

(resolver
 :TicketResolver
 {:with-methods
  {:query (partial ticket-query tickets-connection-info)
   :create (partial ticket-comment-create tickets-connection-info)}
  :paths [:Ticket.Core/Ticket :Ticket.Core/TicketComment]})

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
  :paths [:Ticket.Core/GithubMember]})

(defn as-json [result]
  (json/encode (mapv cn/instance-attributes result)))