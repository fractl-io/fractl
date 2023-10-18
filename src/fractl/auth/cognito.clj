(ns fractl.auth.cognito
  (:require [amazonica.aws.cognitoidp
             :refer [sign-up admin-delete-user admin-get-user
                     admin-update-user-attributes admin-user-global-sign-out change-password
                     confirm-forgot-password confirm-sign-up forgot-password initiate-auth
                     create-group delete-group admin-add-user-to-group admin-remove-user-from-group resend-confirmation-code]]
            [clojure.string :as str]
            [fractl.util.logger :as log]
            [fractl.auth.core :as auth]
            [fractl.auth.jwt :as jwt]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.util.http :as uh]))

(def ^:private tag :cognito)

(defmethod auth/make-client tag [{:keys [access-key secret-key region] :as _config}]
  (or (auth/client-key _config)
      {:access-key access-key
       :secret-key secret-key
       :endpoint region}))

(defn make-jwks-url [region user-pool-id]
  (str "https://cognito-idp."
       region
       ".amazonaws.com/"
       user-pool-id
       "/.well-known/jwks.json"))

(defn- get-error-msg-and-log [cognito-exception]
  (let [error-msg (ex-message cognito-exception)]
    (log/error cognito-exception)
    (subs
     error-msg
     0
     (or (str/index-of error-msg  "(Service: AWSCognitoIdentityProvider")
         (count error-msg)))))

(defmethod auth/make-authfn tag [_config]
  (let [{:keys [region user-pool-id] :as _aws-config} (uh/get-aws-config)]
    (fn [_req token]
      (try
        (jwt/verify-and-extract
         (make-jwks-url region user-pool-id)
         token)
        (catch Exception e)))))

(defmethod auth/user-login tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)]
    (try
      (initiate-auth (auth/make-client (merge req aws-config))
                     :auth-flow "USER_PASSWORD_AUTH"
                     :auth-parameters {"USERNAME" (:Username event)
                                       "PASSWORD" (:Password event)}
                     :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defn- sign-up-user [req aws-config client-id user-pool-id whitelist? user]
  (let [{:keys [Name FirstName LastName Password Email]} user]
    (try
      (sign-up
       (auth/make-client (merge req aws-config))
       :client-id client-id
       :password Password
       :user-attributes [["given_name" FirstName]
                         ["family_name" LastName]
                         ["email" Email]
                         ["name" Name]]
       :username Email)
      (catch com.amazonaws.services.cognitoidp.model.UsernameExistsException ex
        (log/error ex))
      (catch com.amazonaws.services.cognitoidp.model.CodeDeliveryFailureException ex
        (log/error ex))
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/upsert-user tag [{:keys [instance] :as req}]
  (let [{:keys [client-id user-pool-id whitelist?] :as aws-config} (uh/get-aws-config)]
    (case (last (li/split-path (cn/instance-type instance)))
      ;; Create User
      :User
      (let [user instance]
        (sign-up-user req aws-config client-id user-pool-id whitelist? user)
        nil)

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
          (admin-update-user-attributes
           (auth/make-client (merge req aws-config))
           :username cognito-username
           :user-pool-id user-pool-id
           :user-attributes [["given_name" FirstName]
                             ["family_name" LastName]
                             ["custom:github_org" Org]
                             ["custom:github_token" Token]
                             ["custom:github_username" Username]])
          ;; Refresh credentials
          (initiate-auth
           (auth/make-client (merge req aws-config))
           :auth-flow "REFRESH_TOKEN_AUTH"
           :auth-parameters {"USERNAME" cognito-username
                             "REFRESH_TOKEN" refresh-token}
           :client-id client-id)
          (catch Exception e
            (throw (Exception. (get-error-msg-and-log e))))))

      nil)))

(defmethod auth/refresh-token tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)
        cognito-username (get-in req [:user :username])
        refresh-token (:RefreshToken event)]
    (try
      (initiate-auth
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
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config)]
    (try
      (admin-user-global-sign-out
       (auth/make-client (merge req aws-config))
       :user-pool-id user-pool-id
       :username (:username sub))
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/delete-user tag [{:keys [instance] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config)]
    (when-let [email (:Email instance)]
      (try
        (admin-delete-user
         :username email
         :user-pool-id user-pool-id)
        (catch Exception e
          (throw (Exception. (get-error-msg-and-log e))))))))

(defmethod auth/get-user tag [{:keys [user] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config)
        resp (admin-get-user
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

(defmethod auth/resend-confirmation-code tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)]
    (try
      (resend-confirmation-code
       (auth/make-client (merge req aws-config))
       :username (:Username event)
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/confirm-sign-up tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)
        {:keys [Username ConfirmationCode]} event]
    (try
      (confirm-sign-up
       (auth/make-client (merge req aws-config))
       :username Username
       :confirmation-code ConfirmationCode
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/forgot-password tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)]
    (try
      (forgot-password
       (auth/make-client (merge req aws-config))
       :username (:Username event)
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/confirm-forgot-password tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)
        {:keys [Username ConfirmationCode Password]} event]
    (try
      (confirm-forgot-password
       (auth/make-client (merge req aws-config))
       :username Username
       :confirmation-code ConfirmationCode
       :client-id client-id
       :password Password)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/change-password tag [{:keys [event] :as req}]
  (let [aws-config (uh/get-aws-config)
        {:keys [AccessToken CurrentPassword NewPassword]} event]
    (try
      (change-password
       (auth/make-client (merge req aws-config))
       :access-token AccessToken
       :previous-password CurrentPassword
       :proposed-password NewPassword)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/create-role tag [{:keys [role-name] :as req}]
  (try
    (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)]
      (create-group
       (auth/make-client (merge req cfg))
       :group-name role-name
       :user-pool-id user-pool-id))
    (catch com.amazonaws.services.cognitoidp.model.GroupExistsException ex
      req)))

(defmethod auth/delete-role tag [{:keys [client role-name] :as req}]
  (try
    (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)]
      (delete-group
       (or client (auth/make-client (merge req cfg)))
       :group-name role-name
       :user-pool-id user-pool-id))
    (catch com.amazonaws.services.cognitoidp.model.ResourceNotFoundException ex
      req)))

(defmethod auth/add-user-to-role tag [{:keys [client role-name username] :as req}]
  (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)]
    (admin-add-user-to-group
     (or client (auth/make-client (merge req cfg)))
     :group-name role-name
     :username username
     :user-pool-id user-pool-id)))

(defmethod auth/remove-user-from-role tag [{:keys [client role-name username] :as req}]
  (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)]
    (admin-remove-user-from-group
     (or client (auth/make-client (merge req cfg)))
     :group-name role-name
     :username username
     :user-pool-id user-pool-id)))
