(ns fractl.resolver.aws
  "AWS Resolver"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [clojure.data.json :as json]
            [clojure.string :as string]))

(def sns-client (aws/client {:api :sns}))

;; guard against invalid :request map
(def validator (aws/validate-requests sns-client true))

;; what ops are available
(def ops (aws/ops sns-client))

(def sorter (-> (aws/ops sns-client) keys sort))

(defn create-sms-sandbox-phno
  [ph-no]
  (aws/invoke sns-client {:op      :CreateSMSSandboxPhoneNumber
                          :request {:PhoneNumber (str ph-no)
                                    :LanguageCode "en-US"}}))

(defn verify-sms-sandbox-phno
  [ph-no otp]
  (aws/invoke sns-client {:op :VerifySMSSandboxPhoneNumber
                          :request {:PhoneNumber (str ph-no)
                                    :OneTimePassword (str otp)}}))

(defn publish-message-sms
  [ph-no message]
  (aws/invoke sns-client {:op :Publish
                          :request {:Message (str message)
                                    :PhoneNumber (str ph-no)}}))

(defn publish-message-email
  "Publish Email to a topic defined in AWS.
  Current assumption is there is a single topic defined."
  [message]
  (let [topics (get (aws/invoke sns-client {:op :ListTopics}) :Topics)
        single-topic-arn (get (first topics) :TopicArn)]
    (aws/invoke sns-client {:op      :Publish
                            :request {:Message  (str message)
                                      :TopicArn single-topic-arn}})))

(defn validate-token-and-get-arn
  "Even if device token is registered, an EndpointArn is returned."
  [token]
  (let [platform-applications (get (aws/invoke sns-client {:op :ListPlatformApplications}) :PlatformApplications)
        platform-application-arn (get (first platform-applications) :PlatformApplicationArn)
        generate-arn (aws/invoke sns-client {:op :CreatePlatformEndpoint
                                             :request {:PlatformApplicationArn platform-application-arn
                                                       :Token token}})]
    (get generate-arn :EndpointArn)))

(defn push-notification
  "Push notifications reads platform applications,
  and currently assumes one platform application will be used for
  multiple endpoints (which is the general convention).

  After TargetARN is generated from validate-token-and-get-arn,
  that should be passed onto this."
  [target-arn title body]
  (let [platform-applications (get (aws/invoke sns-client {:op :ListPlatformApplications}) :PlatformApplications)
        platform-application-arn (get (first platform-applications) :PlatformApplicationArn)
        proper-data (read-string (apply str (filter #(not= % \:)
                                                    (json/write-str {:GCM {:notification
                                                                           {:title (str title)
                                                                            :body  (str body)}}}))))]
    ;; Weak checking, as we don't need it to be strict.
    (if (string/includes? platform-application-arn "GCM")
      (aws/invoke sns-client {:op      :Publish
                              :request {:Message          (json/write-str {"GCM" (json/write-str (get proper-data "GCM"))})
                                        :TargetArn        (str target-arn)
                                        :MessageStructure "json"}})
      (u/throw-ex "Platform application should be GCM (Firebase Cloud Messaging)"))))

(defn- sns-eval
  [inst]
  (let [message (:Message inst)]
    (case (keyword (:Type inst))
      :email (publish-message-email message)
      :sms (publish-message-sms (:PhoneNumber inst) message)
      :validate-or-generate-arn (validate-token-and-get-arn (:Token inst))
      :push-notification (push-notification (:TargetArn inst) (:Title inst) (:Body inst))
      (u/throw-ex (str "invalid sns type - " (:Type inst))))))

(def ^:private resolver-fns
  {:eval {:handler sns-eval}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
