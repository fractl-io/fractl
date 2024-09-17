(component :Selfservice.Slack)

(require '[clojure.string :as s])
(require '[agentlang.component :as cn])
(require '[agentlang.util :as u])
(require '[agentlang.util.http :as http])
(require '[agentlang.datafmt.json :as json])
(require '[agentlang.lang.internal :as li])

(entity
 :Chat
 {:channel {:type :String
            :guid true
            :default (System/getenv "SLACK_CHANNEL_ID")}
  :text :String
  :mrkdwn {:type :Boolean :default true}
  :thread {:type :String :optional true}})

(entity
 :Approval
 {:thread {:type :String :optional true}
  :channel {:type :String :optional true}
  :data {:type :Any :optional true}})

(def slack-api-key (System/getenv "SLACK_API_KEY"))

(def slack-base-url "https://slack.com/api")

(defn get-url [endpoint] (str slack-base-url endpoint))

(defn- handle-response [response result]
  (let [status (:status response)
        body (:body response)]
    (if (<= 200 status 299)
      (let [output-decoded (json/decode body)]
        (if (:ok output-decoded)
          (assoc result :thread (:ts output-decoded))
          (throw (ex-info "Request failed. " output-decoded))))
      (throw (ex-info "Request failed. " {:status status :body body})))))

(def ^:private http-opts {:headers {"Authorization" (str "Bearer " slack-api-key)
                                  "Content-Type" "application/json"}})

(defn- create-chat [api-name instance]
  (let [data (dissoc instance :-*-type-*- :type-*-tag-*- :thread)
        url (get-url (str "/" api-name))
        response (http/do-post url http-opts data)]
    (handle-response response instance)))

(defn create-entity [instance]
  (let [[c n] (li/split-path (cn/instance-type instance))]
    (if (= n :Chat)
      (create-chat "chat.postMessage" instance)
      instance)))

(defn- normalize-email [email]
  (if-let [idx (s/index-of email ":")]
    (if-let [idx2 (s/index-of email "|")]
      (subs email (inc idx) idx2)
      (subs email (inc idx) (dec (count email))))
    email))

(defn- parse-approval-data [s]
  (when-let [idx (s/index-of s "{")]
    (let [data (json/decode (subs s idx))]
      (if-let [email (:Email data)]
        (assoc data :Email (normalize-email email))
        data))))

(defn- extract-approval-instance [response]
  (let [status (:status response)
        body (:body response)]
    (when (= 200 status)
      (let [output-decoded (json/decode body)]
        (when (:ok output-decoded)
          (let [messages (:messages output-decoded)]
            (when (and (= 2 (count messages)) (= "approve" (:text (second messages))))
              (u/trace (cn/make-instance :Selfservice.Slack/Approval {:data (parse-approval-data (:text (first messages)))})))))))))

(defn- extract-query-params [where]
  (when (= :and (first where))
    (loop [xs (rest where), ts nil, channel nil]
      (if-let [[opr tag val] (first xs)]
        (when (= := opr)
          (case tag
            :thread (recur (rest xs) val channel)
            :channel (recur (rest xs) ts val)))
        (when (and ts channel)
          [ts channel])))))

(defn get-entity [[[_ n] {where :where}]]
  (when (= n :Approval)
    (when-let [[ts channel] (extract-query-params where)]
      (let [url (get-url (str "/conversations.replies?ts=" ts "&channel=" channel))
            f (fn [] (Thread/sleep (* 10 1000)) (http/do-get url http-opts))]
        (loop [response (f)]
          (if-let [r (extract-approval-instance response)]
            [r]
            (recur (f))))))))

(resolver
 :Selfservice.Slack/Resolver
 {:with-methods {:create create-entity
                 :query get-entity}
  :paths [:Selfservice.Slack/Chat :Selfservice.Slack/Approval]})
