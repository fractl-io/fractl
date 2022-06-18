(ns fractl.resolver.aws
  "AWS Resolver"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [fractl.util.logger :as log]))

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
                                  ;; For FCM currently only use PlatformCredential
                                  :Attributes (walk/stringify-keys {:PlatformCredential attributes})}})]
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
  [topic-arn protocol endpoint]
  (let [subscribe (aws/invoke sns-client {:op      :Subscribe
                                          :request {:TopicArn topic-arn
                                                    :Protocol protocol
                                                    :Endpoint endpoint}})]
    (get subscribe :SubscriptionArn)))

(defn confirm-created-subscription
  [topic-arn token]
  (let [subscribe (aws/invoke sns-client {:op      :ConfirmSubscription
                                          :request {:TopicArn topic-arn
                                                    :Token    token}})]
    (get subscribe :SubscriptionArn)))

(defn- upsert-inst [id key handle inst]
  (u/call-and-set
    db
    #(assoc
       @db id
           (assoc inst key handle)))
  (assoc inst key handle))

(defn upsert-platform-application
  [inst]
  (let [id (cn/id-attr inst)
        name (:Name inst)
        platform (:Platform inst)
        ;; Currently take :PlatformCredential as attributes
        ;attributes (:Attributes inst)
        attributes (:PlatformCredential inst)
        platform-arn (create-platform-application name platform attributes)]
    (upsert-inst id :PlatformApplicationArn platform-arn inst)))

(defn upsert-platform-endpoint
  [inst]
  (let [id (cn/id-attr inst)
        platform-arn (:PlatformApplicationArn inst)
        token (:Token inst)
        platform-endpoint (create-platform-endpoint platform-arn token)]
    (upsert-inst id :EndpointArn platform-endpoint inst)))

(defn upsert-topic
  [inst]
  (let [id (cn/id-attr inst)
        name (:Name inst)
        topic (create-topic name)]
    (upsert-inst id :TopicArn topic inst)))

(defn upsert-subscription
  [inst]
  (let [id (cn/id-attr inst)
        topic-arn (:TopicArn inst)
        protocol (:Protocol inst)
        endpoint (:Endpoint inst)
        subscription (create-subscription topic-arn protocol endpoint)]
    (upsert-inst id :SubscriptionArn subscription inst)))

(defn upsert-confirm-subscription
  [inst]
  (let [id (cn/id-attr inst)
        topic-arn (:TopicArn inst)
        token (:Token inst)
        c-subscription (confirm-created-subscription topic-arn token)]
    (upsert-inst id :SubscriptionArn c-subscription inst)))

(defn upsert-functionalities [inst]
  (log/info (str "Inst is: " inst))
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
