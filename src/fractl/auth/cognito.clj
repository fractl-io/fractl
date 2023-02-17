(ns fractl.auth.cognito
  (:require [amazonica.aws.cognitoidp :as cognito]
            [amazonica.core :refer [ex->map]]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [fractl.auth.core :as auth]
            [fractl.auth.jwt :as jwt]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.util.http :as uh]))

(def ^:private tag :cognito)

(defmethod auth/make-client tag [{:keys [access-key secret-key region] :as _config}]
  {:access-key access-key
   :secret-key secret-key
   :endpoint region})

(defn make-jwks-url [region user-pool-id]
  (str "https://cognito-idp."
       region
       ".amazonaws.com/"
       user-pool-id
       "/.well-known/jwks.json"))

(defn- get-error-msg-and-log [cognito-exception]
  (let [error-msg (:message (ex->map cognito-exception))]
    (log/error cognito-exception)
    (log/error (ex->map cognito-exception))
    (subs
     error-msg
     0
     (or (str/index-of error-msg  "(Service: AWSCognitoIdentityProvider")
         (count error-msg)))))

(defmethod auth/make-authfn tag [_config]
  (let [{:keys [region user-pool-id] :as _aws-config} (uh/get-aws-config false)]
    (fn [_req token]
      (try
        (jwt/verify-and-extract
         (make-jwks-url region user-pool-id)
         token)
        (catch Exception e
          (log/error e)
          (throw e))))))


(defmethod auth/user-login tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config false)]
    (try
      (cognito/initiate-auth (auth/make-client (merge req aws-config))
                             :auth-flow "USER_PASSWORD_AUTH"
                             :auth-parameters {"USERNAME" (:Username event)
                                               "PASSWORD" (:Password event)}
                             :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/upsert-user tag [{:keys [instance] :as req}]
  (let [{:keys [client-id user-pool-id] :as aws-config} (uh/get-aws-config false)]
    (case (last (li/split-path (cn/instance-type instance)))
    ;; Create User
      :User
      (let [user instance
            {:keys [Name FirstName LastName Password Email]} user]
        (try
          (cognito/sign-up
           (auth/make-client (merge req aws-config))
           :client-id client-id
           :password Password
           :user-attributes [["given_name" FirstName]
                             ["family_name" LastName]
                             ["email" Email]
                             ["name" Name]]
           :username Email)
          user
          (catch Exception e
            (throw (Exception. (get-error-msg-and-log e))))))

    ;; Update user
      :UpdateUser
      (let [user-details (:UserDetails instance)
            cognito-username (get-in req [:user :username])
            inner-user-details (:User user-details)
            {:keys [FirstName LastName]} inner-user-details
            github-details (get-in user-details [:OtherDetails :GitHub])
            {:keys [Username Org Token]} github-details
            refresh-token (get-in user-details [:OtherDetails :RefreshToken])]
        (try
          (cognito/admin-update-user-attributes
           (auth/make-client (merge req aws-config))
           :username cognito-username
           :user-pool-id user-pool-id
           :user-attributes [["given_name" FirstName]
                             ["family_name" LastName]
                             ["custom:github_org" Org]
                             ["custom:github_token" Token]
                             ["custom:github_username" Username]])
        ;; Refresh credentials
          (cognito/initiate-auth
           (auth/make-client (merge req aws-config))
           :auth-flow "REFRESH_TOKEN_AUTH"
           :auth-parameters {"USERNAME" cognito-username
                             "REFRESH_TOKEN" refresh-token}
           :client-id client-id)
          (catch Exception e
            (throw (Exception. (get-error-msg-and-log e))))))

      nil)))

(defmethod auth/refresh-token tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config false)
        cognito-username (get-in req [:user :username])
        refresh-token (:RefreshToken event)]
    (try
      (cognito/initiate-auth
       (auth/make-client (merge req aws-config))
       :auth-flow "REFRESH_TOKEN_AUTH"
       :auth-parameters {"USERNAME" cognito-username
                         "REFRESH_TOKEN" refresh-token}
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/session-user tag [all-stuff-map]
  (let [user-details (get-in all-stuff-map [:request :identity])]
    {:github-username (:custom:github_username user-details)
     :github-token (:custom:github_token user-details)
     :github-org (:custom:github_org user-details)
     :email (or (:email user-details) (:username user-details))
     :sub (:sub user-details)
     :username (or (:cognito:username user-details) (:sub user-details))}))

(defmethod auth/session-sub tag [req]
  (auth/session-user req))

(defmethod auth/user-logout tag [{:keys [sub] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config false)]
    (try
      (cognito/admin-user-global-sign-out
       (auth/make-client (merge req aws-config))
       :user-pool-id user-pool-id
       :username (:username sub))
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/delete-user tag [{:keys [instance] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config false)]
    (when-let [email (:Email instance)]
      (try
        (cognito/admin-delete-user
         (auth/make-client (merge req aws-config))
         :username email
         :user-pool-id user-pool-id)
        (catch Exception e
          (throw (Exception. (get-error-msg-and-log e))))))))

(defmethod auth/get-user tag [{:keys [user] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config false)
        resp (cognito/admin-get-user
              (auth/make-client (merge req aws-config))
              :username (:username user)
              :user-pool-id user-pool-id)
        user-attributes (:user-attributes resp)
        user-attributes-map (reduce
                             (fn [attributes-map attribute-map]
                               (assoc attributes-map (keyword (:name attribute-map)) (:value attribute-map)))
                             {}
                             user-attributes)
        {:keys [custom:github_username custom:github_token custom:github_org
                given_name family_name email]} user-attributes-map]
    {:GitHub {:Username custom:github_username
              :Token custom:github_token
              :Org custom:github_org}
     :FirstName given_name
     :LastName family_name
     :Email email}))

(defmethod auth/forgot-password tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config false)]
    (try
      (cognito/forgot-password
       (auth/make-client (merge req aws-config))
       :username (:Username event)
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/confirm-forgot-password tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config false)
        {:keys [Username ConfirmationCode Password]} event]
    (try
      (cognito/confirm-forgot-password
       (auth/make-client (merge req aws-config))
       :username Username
       :confirmation-code ConfirmationCode
       :client-id client-id
       :password Password)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/change-password tag [{:keys [event] :as req}]
  (let [aws-config (uh/get-aws-config false)
        {:keys [AccessToken CurrentPassword NewPassword]} event]
    (try
      (cognito/change-password
       (auth/make-client (merge req aws-config))
       :access-token AccessToken
       :previous-password CurrentPassword
       :proposed-password NewPassword)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))
