(ns fractl.resolver.sns
  "SNS Resolver"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def sns-client (aws/client {:api :sns}))

;; guard against invalid :request map
(def validator (aws/validate-requests sns-client true))

;; what ops are available
(def ops (aws/ops sns-client))

(def sorter (-> (aws/ops sns-client) keys sort))

(defn create-sms-sandbox-phno
  [ph-no]
  (aws/invoke sns-client {:op      :CreateSMSSandboxPhoneNumber
                          :request {:PhoneNumber  (str ph-no)
                                    :LanguageCode "en-US"}}))

(defn verify-sms-sandbox-phno
  [ph-no otp]
  (aws/invoke sns-client {:op      :VerifySMSSandboxPhoneNumber
                          :request {:PhoneNumber     (str ph-no)
                                    :OneTimePassword (str otp)}}))

(defn publish-message-sms
  [ph-no message]
  (aws/invoke sns-client {:op      :Publish
                          :request {:Message     (str message)
                                    :PhoneNumber (str ph-no)}}))

(defn publish-message-email
  "Publish Email to a topic defined in AWS.
  If message is only sent, assumption of single topic is made."
  ([message topic-arn]
   (aws/invoke sns-client {:op      :Publish
                           :request {:Message  (str message)
                                     :TopicArn topic-arn}}))
  ([message]
   (let [topics (get (aws/invoke sns-client {:op :ListTopics}) :Topics)
         single-topic-arn (get (first topics) :TopicArn)]
     (aws/invoke sns-client {:op      :Publish
                             :request {:Message  (str message)
                                       :TopicArn single-topic-arn}}))))

(defn push-notification-to-endpoint
  "Pushes notifications to platforms endpoint.
  Current assumption is GCM."
  [target-arn title body]
  (let [proper-data (read-string (apply str (filter #(not= % \:)
                                                    (json/write-str {:GCM {:notification
                                                                           {:title (str title)
                                                                            :body  (str body)}}}))))]
    (aws/invoke sns-client {:op      :Publish
                            :request {:Message          (json/write-str {"GCM" (json/write-str (get proper-data "GCM"))})
                                      :TargetArn        (str target-arn)
                                      :MessageStructure "json"}})
    (u/throw-ex "Platform application should be GCM (Firebase Cloud Messaging)")))

(defn push-notification-to-topic
  "Pushes notifications to topic.

  Current assumption is GCM."
  [topic-arn title body]
  (let [proper-data (read-string (apply str (filter #(not= % \:)
                                                    (json/write-str {:GCM {:notification
                                                                           {:title (str title)
                                                                            :body  (str body)}}}))))]
    (aws/invoke sns-client {:op      :Publish
                            :request {:Message          (json/write-str {"GCM" (json/write-str (get proper-data "GCM"))})
                                      :TopicArn        (str topic-arn)
                                      :MessageStructure "json"}})
    (u/throw-ex "Platform application should be GCM (Firebase Cloud Messaging)")))

(defn- sns-eval
  [inst]
  (let [message (:Message inst)]
    (case (keyword (:Type inst))
      :email (publish-message-email message (when (get inst :TopicArn) (:TopicArn inst)))
      :sms (publish-message-sms (:PhoneNumber inst) message)
      :push-notification-to-endpoint (push-notification-to-endpoint (:TargetArn inst) (:Title inst) (:Body inst))
      :push-notification-to-topic (push-notification-to-topic (:TopicArn inst) (:Title inst) (:Body inst))
      :create-sandbox-number (create-sms-sandbox-phno (:PhoneNumber inst))
      :verify-sandbox-phone-number (verify-sms-sandbox-phno (:PhoneNumber inst) (:OTP inst))
      (u/throw-ex (str "invalid sns type - " (:Type inst))))))

(def ^:private resolver-fns
  {:eval {:handler sns-eval}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
