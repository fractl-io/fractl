(ns fractl.resolver.aws
  "AWS Resolver"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def ^:private db (u/make-cell {}))

(def sns-client (aws/client {:api :sns}))

;; guard against invalid :request map
(def validator (aws/validate-requests sns-client true))

;; what ops are available
(def ops (aws/ops sns-client))

(def sorter (-> (aws/ops sns-client) keys sort))

(defn create-platform-application
  "Given a name, platform type (ADM,APNS, APNS_SANDBOX, GCM) and map of strings
   for attributes create a platform application."
  [name platform attributes]
  (let [platform-arn (aws/invoke
                       sns-client
                       {:op      :CreatePlatformApplication
                        :request {:Name       name
                                  :Platform   platform
                                  :Attributes [(walk/stringify-keys attributes)]}})]
    (get platform-arn :PlatformApplicationArn)))

(defn create-platform-endpoint
  "Takes a PlatformApplicationArn, device Token and CustomUserData (optional)
   to create platform endpoint."
  ([platform-arn token custom-user-data]
   (let [platform-endpoint (aws/invoke sns-client {:op      :CreatePlatformEndpoint
                                                   :request {:PlatformApplicationArn platform-arn
                                                             :Token                  token
                                                             :CustomUserData         custom-user-data}})]
     (get platform-endpoint :EndpointArn)))
  ([platform-arn token]
   (let [platform-endpoint (aws/invoke sns-client {:op      :CreatePlatformEndpoint
                                                   :request {:PlatformApplicationArn platform-arn
                                                             :Token                  token}})]
     (get platform-endpoint :EndpointArn))))

(defn create-topic
  "Create a topic"
  ([name attributes]
   (let [topic (aws/invoke sns-client {:op      :CreateTopic
                                       :request {:Name       name
                                                 :Attributes [(walk/stringify-keys attributes)]}})]
     (get topic :TopicArn)))
  ([name]
   (let [topic (aws/invoke sns-client {:op      :CreateTopic
                                       :request {:Name name}})]
     (get topic :TopicArn))))

(defn create-subscription
  [topic-arn protocol]
  (let [subscribe (aws/invoke sns-client {:op      :Subscribe
                                          :request {:TopicArn topic-arn
                                                    :Protocol protocol}})]
    (get subscribe :SubscriptionArn)))

(defn confirm-created-subscription
  [topic-arn token]
  (let [subscribe (aws/invoke sns-client {:op      :ConfirmSubscription
                                          :request {:TopicArn topic-arn
                                                    :Token    token}})]
    (get subscribe :SubscriptionArn)))

(defn- upsert-inst [id created inst]
  (u/call-and-set
    db
    #(assoc
       @db id
           [created inst]))
  inst)

(defn- update-in-inst [id handle key]
  (when-let [[created inst]
             (get @db id)]
    (upsert-inst
      id created (assoc inst key handle))))

(defn upsert-platform-application
  [inst]
  (let [id (:Id inst)
        name (:Name inst)
        platform (:Platform inst)
        attributes (:Attributes inst)
        platform-arn (create-platform-application name platform attributes)]
    (update-in-inst id platform-arn :PlatformApplicationArn)))

(defn upsert-platform-endpoint
  [inst]
  (let [id (:Id inst)
        platform-arn (:PlatformApplicationArn inst)
        token (:Token inst)
        platform-endpoint (create-platform-endpoint platform-arn token)]
    (update-in-inst id platform-endpoint :EndpointArn)))


(defn upsert-topic
  [inst]
  (let [id (:Id inst)
        name (:Name inst)
        topic (create-topic name)]
    (update-in-inst id topic :TopicArn)))

(defn upsert-subscription
  [inst]
  (let [id (:Id inst)
        topic-arn (:TopicArn inst)
        protocol (:Protocol inst)
        subscription (create-subscription topic-arn protocol)]
    (update-in-inst id subscription :SubscriptionArn)))

(defn upsert-confirm-subscription
  [inst]
  (let [id (:Id inst)
        topic-arn (:TopicArn inst)
        token (:Token inst)
        c-subscription (confirm-created-subscription topic-arn token)]
    (update-in-inst id c-subscription :SubscriptionArn)))

(defn upsert-functionalities [inst]
  (case (keyword (:Type inst))
    :CreatePlatformApplication (upsert-platform-application inst)
    :CreatePlatformEndpoint (upsert-platform-endpoint inst)
    :CreateTopic (upsert-topic inst)
    :Subscribe (upsert-subscription inst)
    :ConfirmSubscription (upsert-confirm-subscription inst)
    (u/throw-ex (str "invalid Type - " (:Type inst)))))

(def ^:private resolver-fns
  {:upsert {:handler upsert-functionalities}})

(defn make
  "Create and return an aws resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
