(ns fractl.auth.okta
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.http :as http]
            [fractl.util.logger :as log]
            [fractl.util.seq :as us]
            [fractl.datafmt.json :as json]
            [fractl.global-state :as gs]
            [fractl.auth.jwt :as jwt]
            [fractl.auth.core :as auth]))

(def ^:private tag :okta)

;; config required for okta/auth
{:authentication {:service :okta
                  :superuser-email "<email>"
                  :domain "<domain>"
                  :auth-server "<auth-server-name>"
                  :client-id "<client-id>"}}

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
      (catch Exception e
        (log/warn e)))))

(def ^:private auth-config (atom nil))
(def ^:private login-redirect-uri "http://localhost:8080/login/callback")

(defn- get-config []
  (or @auth-config
      (let [ac (:authentication (gs/get-app-config))]
        (reset! auth-config (assoc ac :login-redirect-uri (http/url-encode login-redirect-uri))))))

(defn- fetch-id-token [{domain :domain
                        client-id :client-id
                        auth-server :auth-server
                        login-redirect-uri :login-redirect-uri :as config} login-result]
  (let [session-token (:sessionToken login-result)
        state (us/generate-code 10)
        nonce (str "n-" (us/generate-code 3) "_" (us/generate-code 6))
        url (str "https://" domain "/oauth2/" auth-server "/v1/authorize?client_id=" client-id
                 "&response_type=id_token&scope=openid&prompt=none&redirect_uri=" login-redirect-uri
                 "&state=" state "&nonce=" nonce "&sessionToken=" session-token)
        result (http/do-get url {:follow-redirects false})]
    (if (= 302 (:status result))
      (let [loc (:location (:headers result))
            s (subs loc (inc (s/index-of loc "#")))
            login-info (http/form-decode s)]
        (when (not= state (:state login-info))
          (u/throw-ex (str "okta/authorize failed with state mismatch - " state " <> " (:state login-info))))
        {:authentication-result
         {:id-token (:id_token login-info) :state state :set-cookie (:set-cookie (:headers result))}})
      (u/throw-ex (str "okta/authorize call failed. expected redirect, not " (:status result))))))

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

(defmethod auth/upsert-user tag [req]
  (:instance req))

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

(defmethod auth/user-logout tag [{domain :domain auth-server :auth-server req :request}]
  (let [redirect-uri (http/url-encode "http://localhost:8080/")
        id-token (jwt/remove-bearer (get (:headers req) "authorization"))
        url (str "https://" domain "/oauth2/" auth-server "/v1/logout?"
                 "id_token_hint=" id-token "&post_logout_redirect_uri=" redirect-uri)
        resp (http/do-get url {:follow-redirects false})]
    (if (= 200 (:status resp))
      :bye
      (str "logout failed with status " (:status resp)))))

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
