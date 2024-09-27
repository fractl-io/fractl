(component
 :Slack.Core
 {:clj-import '[(:require [clojure.string :as s]
                          [agentlang.component :as cn]
                          [agentlang.util :as u]
                          [agentlang.util.http :as http]
                          [agentlang.util.logger :as log]
                          [agentlang.datafmt.json :as json]
                          [agentlang.evaluator :as ev]
                          [agentlang.lang.internal :as li])]})

(entity
 :Chat
 {:channel {:type :String
            :guid true
            :default (System/getenv "SLACK_CHANNEL_ID")}
  :text :String
  :response {:type :String :optional true}
  :data {:type :Any :optional true}
  :mrkdwn {:type :Boolean :default true}
  :thread {:type :String :optional true}})

(entity
 :Approval
 {:thread {:type :String :optional true}
  :channel {:type :String :optional true}
  :approved {:type :Boolean :default false}
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

(defn- extract-approval [response]
  (let [status (:status response)
        body (:body response)]
    (when (= 200 status)
      (let [output-decoded (json/decode body)]
        (when (:ok output-decoded)
          (let [messages (:messages output-decoded)]
            (when (>= (count messages) 2)
              (let [r (first
                       (ev/safe-eval
                        {:Slack.Core/InvokeResponseClassifier
                         {:UserInstruction
                          (s/lower-case (s/trim (:text (second messages))))}}))]
                (log/debug (str "slack-resolver/extract-approval: " r))
                r))))))))

(defn wait-for-reply [{channel :channel ts :thread}]
  (let [url (get-url (str "/conversations.replies?ts=" ts "&channel=" channel))
        f (fn [] (Thread/sleep (* 10 1000)) (http/do-get url http-opts))
        r
        (loop [response (f), retries 50]
          (if (zero? retries)
            "reject"
            (if-let [r (extract-approval response)]
              r
              (recur (f) (dec retries)))))]
    (log/debug (str "slack-resolver/wait-for-reply: " r))
    r))

(defn- normalize-email [email]
  (if-let [idx (s/index-of email ":")]
    (if-let [idx2 (s/index-of email "|")]
      (subs email (inc idx) idx2)
      (subs email (inc idx) (dec (count email))))
    email))

(defn- parse-approval-data [s]
  (let [r (first
           (ev/safe-eval
            {:Slack.Core/InvokeDataExtractor
             {:UserInstruction s}}))]
    (log/debug (str "slack-resolver/parse-approval-data: " s ", result: " r))
    r)
  #_(when-let [idx (s/index-of s "{")]
      (let [data (json/decode (subs s idx))]
        (if-let [email (:Email data)]
          (assoc data :Email (normalize-email email))
          data))))

(defn- create-chat [api-name instance]
  (let [data (dissoc instance :-*-type-*- :type-*-tag-*- :thread)
        url (get-url (str "/" api-name))
        response (http/do-post url http-opts data)
        new-instance (handle-response response instance)]
    (assoc new-instance :response (wait-for-reply new-instance)
           :data (parse-approval-data (:text instance)))))

(defn create-entity [instance]
  (let [[c n] (li/split-path (cn/instance-type instance))]
    (if (= n :Chat)
      (create-chat "chat.postMessage" instance)
      instance)))

(defn- extract-approval-instance [response]
  (let [status (:status response)
        body (:body response)]
    (when (= 200 status)
      (let [output-decoded (json/decode body)]
        (when (:ok output-decoded)
          (let [messages (:messages output-decoded)]
            (when (>= (count messages) 2)
              (let [resp (:text (second messages))
                    approved (= "approve" resp)]
                (cn/make-instance
                 :Slack.Core/Approval
                 {:approved approved
                  :data (parse-approval-data (:text (first messages)))})))))))))

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
        (loop [response (f), retries 50]
          (if (zero? retries)
            [(cn/make-instance :Slack.Core/Approval {})] ; reject approval after `n` retries.
            (if-let [r (extract-approval-instance response)]
              [r]
              (recur (f) (dec retries)))))))))

{:Agentlang.Core/LLM {:Type "openai" :Name "slack-resolver-llm"}}

{:Agentlang.Core/Agent
 {:Name "slack-response-classifier"
  :Type "chat"
  :LLM "slack-resolver-llm"
  :UserInstruction
  "Classify the input text to one of the categories - approve or reject.
For example if the input is `you can join the team`, your response must be `approve`.
If the input is `sorry, can't allow`, your response must be `reject`.
If you are unable to classify the text, simply return `reject`.
(Do not include the ticks (`) in your response).
Now please classify the following text following these rules.\n\n"}}

(event :InvokeResponseClassifier {:meta {:inherits :Agentlang.Core/Inference}})
(inference :InvokeResponseClassifier {:agent "slack-response-classifier"})

{:Agentlang.Core/Agent
 {:Name "slack-data-extactor"
  :Type "chat"
  :LLM "slack-resolver-llm"
  :UserInstruction
  "Extract the user email, github organization and ticket Id from a given text.
For example, if the input text is \"Please approve jane@acme.com to join the github org acme. The request id is 23445.\",
then your response must be `{:Email \"jane@acme.com\" :Org \"acme\" :Id \"23445\"}`."}}

(event :InvokeDataExtractor {:meta {:inherits :Agentlang.Core/Inference}})
(inference :InvokeDataExtractor {:agent "slack-data-extactor"})

(resolver
 :Slack.Core/Resolver
 {:with-methods {:create create-entity
                 :query get-entity}
  :paths [:Slack.Core/Chat :Slack.Core/Approval]})
