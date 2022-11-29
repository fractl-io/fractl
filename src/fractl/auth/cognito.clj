(ns fractl.auth.cognito
  (:require [amazonica.aws.cognitoidp :as aws]
            [amazonica.core :refer [ex->map]]
            [fractl.auth.core :as auth]
            [fractl.auth.jwt :as jwt]
            [cheshire.core :as json]
            [buddy.core.keys :as keys])
  (:import (org.apache.commons.lang3 NotImplementedException)))

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
                     :client-id client-id
                     :auth-parameters {"USERNAME" (:Username event)
                                       "PASSWORD" (:Password event)}))

(defn- user-exists? [username user-pool-id config]
  (try
    (aws/admin-get-user (auth/make-client config)
                        :username username
                        :user-pool-id user-pool-id)
    true
    (catch Exception e
      (let [exception-details (ex->map e)
            {:keys [error-code status-code]} exception-details]
        (if (and (= status-code 400) (= error-code "UserNotFoundException"))
          false
          (throw e))))))

;; right now only creates user, update user is done directly via frontend
(defmethod auth/upsert-user tag [{:keys [client-id user-pool-id] user auth/instance-key :as req}]
  (when (user-exists? (:Email user) user-pool-id req)
    (let [{:keys [Name FirstName LastName Password Email]} user]
      (aws/sign-up
       (auth/make-client req)
       :client-id client-id
       :password Password
       :user-attributes [["given_name" FirstName]
                         ["family_name" LastName]
                         ["email" Email]
                         ["name" Name]]
       :username Email))))

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

;; Frontend should use:
;; `https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_GlobalSignOut.html` 
;; directly for now.
(defmethod auth/user-logout tag [_req]
  (throw (NotImplementedException. "Use 'https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_GlobalSignOut.html'")))

(defmethod auth/delete-user tag [& args]
  (println ">>>>>delete-user" (pr-str args)))
