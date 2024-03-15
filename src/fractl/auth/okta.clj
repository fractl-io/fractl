(ns fractl.auth.okta
  (:require [fractl.util :as u]
            [fractl.util.http :as http]
            [fractl.util.logger :as log]
            [fractl.datafmt.json :as json]
            [fractl.global-state :as gs]
            [fractl.auth.jwt :as jwt]
            [fractl.auth.core :as auth]))

(def ^:private tag :okta)

(defmethod auth/make-client tag [{:keys [domain client-id client-secret auth-server] :as _config}]
  _config)

(def ^:private openid-config (u/make-cell nil))

(defn get-jwks-url [domain auth-server client-id]
  (let [cfg
        (or @openid-config
            (u/safe-set
             openid-config
             (let [resp (http/do-get
                         (str "https://" domain
                              "/oauth2/" auth-server
                              "/.well-known/openid-configuration?client_id=" client-id))]
               (if (= (:status resp) 200)
                 (json/decode (:body resp))
                 (let [msg "Okta jwks fetch failed"]
                   (log/warn (str msg ": " resp))
                   (u/throw-ex (str msg " with status: " (:status resp))))))))]
    (:jwks_uri cfg)))

(defmethod auth/make-authfn tag [{domain :domain auth-server :auth-server client-id :client-id}]
  (fn [_req token]
    (try
      (jwt/verify-and-extract
       (get-jwks-url domain auth-server client-id)
       token)
      (catch Exception e))))

(defn- get-config [] (:auth (gs/get-app-config)))

(defn- fetch-id-token [{domain :domain client-id :client-id} login-result]
  (let [session-token (:sessionToken login-result)
        url (str "https://" domain "/oauth2/v1/authorize?client_id=" client-id
                 "&response_type=id_token&scope=openid&prompt=none&redirect_uri=https%3A%2F%2Ffractl.io/auth/callback&state=Af0ifjslDkj&nonce=n-0S6_WzA2Mj&sessionToken=" session-token)]
    ;; TODO: fetch the id_token from the redirect url and return response in a format extected by fractl.http
    ))

(defmethod auth/user-login tag [{:keys [event] :as req}]
  (let [{domain :domain :as config} (get-config)
        url (str "https://" domain "/api/v1/authn")
        result
        (try
          (http/do-post url {"username" (:Username event)
                             "password" (:Password event)
                             "options"
                             {"multiOptionalFactorEnroll" false
                              "warnBeforePasswordExpired" false}})
          (catch Exception ex
            (log/error ex)))]
    (if (= (:status result) 200)
      (fetch-id-token config (json/decode (:body result)))
      (log/warn (str "login failed: " result)))))

(defmethod auth/upsert-user tag [_]
  (u/throw-ex "auth/upsert-user not implemented for okta"))

(defmethod auth/refresh-token tag [{:keys [event] :as req}]
  ;; TODO: implement refresh-token
  )

(defmethod auth/session-user tag [all-stuff-map]
  (let [user (get-in all-stuff-map [:profile :user])]
    {:email user
     :sub user
     :username user}))

(defmethod auth/session-sub tag [req]
  (auth/session-user req))

(defmethod auth/user-logout tag [_]
  ;; TODO: implement logout
  )

(defmethod auth/delete-user tag [_]
  (u/throw-ex "auth/delete-user not implemented for okta"))

(defmethod auth/get-user tag [_]
  (u/throw-ex "auth/get-user not implemented for okta"))

(defmethod auth/resend-confirmation-code tag [_]
  (u/throw-ex "auth/resend-confirmation-code not implemented for okta"))

(defmethod auth/confirm-sign-up tag [_]
  (u/throw-ex "auth/confirm-sign-up not implemented for okta"))

(defmethod auth/forgot-password tag [_]
  (u/throw-ex "auth/forgot-password not implemented for okta"))

(defmethod auth/confirm-forgot-password tag [_]
  (u/throw-ex "auth/confirm-forgot-password not implemented for okta"))

(defmethod auth/change-password tag [_]
  (u/throw-ex "auth/change-password not implemented for okta"))

(defmethod auth/create-role tag [_]
  (u/throw-ex "auth/create-role not implemented for okta"))

(defmethod auth/delete-role tag [_]
  (u/throw-ex "auth/delete-role not implemented for okta"))

(defmethod auth/add-user-to-role tag [_]
  (u/throw-ex "auth/add-user-to-role not implemented for okta"))

(defmethod auth/remove-user-from-role tag [_]
  (u/throw-ex "auth/remove-user-from-role not implemented for okta"))
