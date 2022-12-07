(ns fractl.auth.cognito
  (:require [amazonica.aws.cognitoidp :as aws]
            [amazonica.core :refer [ex->map]]
            [fractl.auth.core :as auth]
            [fractl.auth.jwt :as jwt]
            [fractl.component :as cn]))

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

(defmethod auth/make-authfn tag [{:keys [region user-pool-id] :as _config}]
  (fn [_req token]
    (jwt/verify-and-extract
     (make-jwks-url region user-pool-id)
     token)))

(defmethod auth/user-login tag [{:keys [client-id event] :as req}]
  (aws/initiate-auth (auth/make-client req)
                     :auth-flow "USER_PASSWORD_AUTH"
                     :auth-parameters {"USERNAME" (:Username event)
                                       "PASSWORD" (:Password event)}
                     :client-id client-id))

(defmethod auth/upsert-user tag [{:keys [client-id user-pool-id event] :as req}]
  (case (last (cn/instance-type event))
    ;; Create User
    :SignUp
    (let [user (:User event)
          {:keys [Name FirstName LastName Password Email]} user]
      (aws/sign-up
       (auth/make-client req)
       :client-id client-id
       :password Password
       :user-attributes [["given_name" FirstName]
                         ["family_name" LastName]
                         ["email" Email]
                         ["name" Name]]
       :username Email)
      user)

    :UpdateUser
    ;; Update user
    (let [user-details (:UserDetails event)
          cognito-username (get-in req [:user :username])
          inner-user-details (:User user-details)
          {:keys [FirstName LastName]} inner-user-details
          github-details (get-in user-details [:OtherDetails :GitHub])
          {:keys [Username Org Token]} github-details
          refresh-token (get-in user-details [:OtherDetails :RefreshToken])]
      (aws/admin-update-user-attributes
       (auth/make-client req)
       :username cognito-username
       :user-pool-id user-pool-id
       :user-attributes [["given_name" FirstName]
                         ["family_name" LastName]
                         ["custom:github_org" Org]
                         ["custom:github_token" Token]
                         ["custom:github_username" Username]])
      ;; Refresh credentials
      (aws/initiate-auth
       (auth/make-client req)
       :auth-flow "REFRESH_TOKEN_AUTH"
       :auth-parameters {"USERNAME" cognito-username
                         "REFRESH_TOKEN" refresh-token}
       :client-id client-id))

    nil))

(defmethod auth/session-user tag [all-stuff-map]
  (let [user-details (get-in all-stuff-map [:request :identity])]
    {:github-username (:custom:github_username user-details)
     :github-token (:custom:github_token user-details)
     :github-org (:custom:github_org user-details)
     :email (:email user-details)
     :sub (:sub user-details)
     :username (:cognito:username user-details)}))

(defmethod auth/session-sub tag [req]
  (auth/session-user req))

(defmethod auth/user-logout tag [{:keys [sub user-pool-id] :as req}]
  (aws/admin-user-global-sign-out
   (auth/make-client req)
   :user-pool-id user-pool-id
   :username (:username sub)))

(defmethod auth/delete-user tag [{:keys [sub user-pool-id] :as req}]
  (aws/admin-delete-user
   (auth/make-client req)
   :username (:username sub)
   :user-pool-id user-pool-id))

(defmethod auth/get-user tag [{:keys [user user-pool-id] :as req}]
  (let [resp (aws/admin-get-user
              (auth/make-client req)
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
