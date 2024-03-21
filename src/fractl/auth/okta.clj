(ns fractl.auth.okta
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.http :as http]
            [fractl.util.logger :as log]
            [fractl.util.seq :as us]
            [fractl.lang.b64 :as b64]
            [fractl.datafmt.json :as json]
            [fractl.global-state :as gs]
            [fractl.auth.jwt :as jwt]
            [fractl.auth.core :as auth]
            [fractl.user-session :as sess]))

(def ^:private tag :okta)

;; config required for okta/auth:
#_{:authentication
   {:service :okta
    :superuser-email "<email>"
    :domain "<domain>"
    :auth-server "<auth-server-name>"
    :client-id "<client-id>"
    :client-secret "<secret>"
    :introspect <boolean> ; call okta introspect api to verify the token, defaults to false
    :auto-refresh-token <boolean> ; default false
    }}

(defmethod auth/make-client tag [_config]
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

(defn- verify-and-extract [{domain :domain auth-server :auth-server client-id :client-id} token]
  (try
    (jwt/verify-and-extract
     (get-jwks-url domain auth-server client-id)
     token)
    (catch Exception e
      (log/warn e))))

(declare introspect)

(defn- refresh-tokens [{domain :domain
                        client-id :client-id
                        client-secret :client-secret
                        auth-server :auth-server
                        login-redirect-uri :login-redirect-uri
                        :as auth-config}
                       token sid]
  (let [url (str "https://" domain "/oauth2/" auth-server "/v1/token")
        req (str "client_id=" client-id "&client_secret=" client-secret
                 "&grant_type=refresh_token&redirect_uri=" login-redirect-uri
                 "&refresh_token=" token)
        resp (http/do-request :post url {"Content-Type" "application/x-www-form-urlencoded"} req)]
    (if (= 200 (:status resp))
      (let [r (us/snake-to-kebab-keys (json/decode (:body resp)))
            data (sess/session-cookie-update-tokens sid r)]
        (if data
          (introspect auth-config [sid data])
          (log/warn (str "failed to refresh tokens for " sid))))
      (do (log/error resp)
          (u/throw-ex (str "failed to refresh access token with error: " (:status resp)))))))

(defn- do-introspect [token-hint domain auth-server client-id client-secret token]
  (let [url (str "https://" domain "/oauth2/" auth-server "/v1/introspect")
        req (str "token=" token "&token_type_hint=" token-hint)
        resp (http/do-request
              :post url
              {"Content-Type" "application/x-www-form-urlencoded"
               "Authorization" (str "Basic " (b64/encode-string (str client-id ":" client-secret)))}
              req)]
    (if (= 200 (:status resp))
      (let [result (json/decode (:body resp))]
        (if (:active result)
          result
          (log/warn (str token-hint " introspect returned inactive state: " result))))
      (log/warn (str token-hint " introspect returned status: " (:status resp))))))

(def ^:private introspect-access-token (partial do-introspect "access_token"))
(def ^:private introspect-refresh-token (partial do-introspect "refresh_token"))

(defn- introspect [{domain :domain
                    auth-server :auth-server
                    client-id :client-id
                    client-secret :client-secret
                    :as auth-config}
                   [sid data]]
  (let [authres (:authentication-result data)
        access-token (:access-token authres)
        resp (introspect-access-token
              domain auth-server client-id client-secret
              access-token)]
    (or resp
        (let [reftok (:refresh-token authres)]
          (when (introspect-refresh-token
                 domain auth-server client-id client-secret
                 reftok)
            (refresh-tokens auth-config reftok sid))))))

(defmethod auth/verify-token tag [auth-config data]
  (if (vector? data)
    (if (:introspect auth-config)
      (introspect auth-config data)
      (verify-and-extract auth-config (:id-token (:authentication-result (second data)))))
    (verify-and-extract auth-config data)))

(defmethod auth/make-authfn tag [auth-config]
  (fn [_ token] (verify-and-extract auth-config token)))

(def ^:private auth-config (atom nil))
(def ^:private login-redirect-uri "http://localhost:8080/authorization-code/callback")

(defn- get-config []
  (or @auth-config
      (let [ac (:authentication (gs/get-app-config))]
        (reset! auth-config (assoc ac :login-redirect-uri (http/url-encode login-redirect-uri))))))

(defn- extract-sid [cookie]
  (when-let [i (s/index-of cookie "sid=")]
    (let [j (s/index-of cookie ";" i)
          sid (subs cookie i j)]
      sid)))

(defn- code-to-tokens [{domain :domain
                        client-id :client-id
                        client-secret :client-secret
                        auth-server :auth-server
                        login-redirect-uri :login-redirect-uri}
                       code]
  (let [url (str "https://" domain "/oauth2/" auth-server "/v1/token")
        req (str "client_id=" client-id "&client_secret=" client-secret
                 "&grant_type=authorization_code&redirect_uri=" login-redirect-uri
                 "&code=" code)
        resp (http/do-request :post url {"Content-Type" "application/x-www-form-urlencoded"} req)]
    (if (= 200 (:status resp))
      (json/decode (:body resp))
      (do (log/error resp)
          (u/throw-ex (str "failed to get access token with error: " (:status resp)))))))

(defn- fetch-tokens [{domain :domain
                      client-id :client-id
                      auth-server :auth-server
                      login-redirect-uri :login-redirect-uri :as config} login-result]
  (let [session-token (:sessionToken login-result)
        state (us/generate-code 10)
        nonce (str "n-" (us/generate-code 3) "_" (us/generate-code 6))
        url (str "https://" domain "/oauth2/" auth-server "/v1/authorize?client_id=" client-id
                 "&response_type=code&scope=openid%20offline_access&prompt=none&redirect_uri=" login-redirect-uri
                 "&state=" state "&nonce=" nonce "&sessionToken=" session-token)
        result (http/do-get url {:follow-redirects false})]
    (if (= 302 (:status result))
      (let [loc (:location (:headers result))
            s (subs loc (inc (s/index-of loc "?")))
            login-info (http/form-decode s)]
        (when (not= state (:state login-info))
          (u/throw-ex (str "okta/authorize failed with state mismatch - " state " <> " (:state login-info))))
        (let [tokens (code-to-tokens config (:code login-info))]
          {:authentication-result
           (merge
            {:state state
             :user-data {:cookie (extract-sid (:set-cookie (:headers result)))}}
            (us/snake-to-kebab-keys tokens))}))
      (do (log/error result)
          (u/throw-ex (str "okta/authorize call failed. expected redirect, not " (:status result)))))))

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
      (fetch-tokens config (json/decode (:body result)))
      (log/warn (str "login failed: " result)))))

(defmethod auth/upsert-user tag [req]
  (:instance req))

(defmethod auth/refresh-token tag [{:keys [event] :as req}]
  ;; TODO: implement refresh-token
  )

(defn- make-authorize-uri [{authorize-uri :authorize-uri
                            authorize-callback-uri :authorize-callback-uri
                            client-id :client-id
                            scope :scope}]
  (str authorize-uri "?client_id=" client-id "&redirect_uri=" (http/url-encode authorize-callback-uri)
       "&response_type=code&scope=" scope "&state=" (us/generate-code 10)))

(defmethod auth/authenticate-session tag [{cookie :cookie
                                           client-uri :client-uri
                                           :as auth-config}]
  (if (sess/lookup-session-cookie-user-data cookie)
    {:status :redirect-found :location client-uri}
    {:status :redirect-found :location (make-authorize-uri auth-config)}))

(defmethod auth/session-user tag [{req :request cookie :cookie :as all-stuff-map}]
  (if cookie
    (let [result (introspect all-stuff-map cookie)
          user (:sub result)]
      {:email user
       :sub user
       :username (or (:username result) user)})
    (let [user (get-in req [:identity :sub])]
      {:email user
       :sub user
       :username user})))

(defmethod auth/handle-auth-callback tag [auth-config]
  )

(defmethod auth/session-sub tag [req]
  (auth/session-user req))

(defmethod auth/user-logout tag [{domain :domain auth-server :auth-server req :request cookie :cookie}]
  (let [redirect-uri (http/url-encode "http://localhost:8080/")
        id-token (if cookie
                   (get-in (second cookie) [:authentication-result :id-token])
                   (jwt/remove-bearer (get (:headers req) "authorization")))
        url (str "https://" domain "/oauth2/" auth-server "/v1/logout?"
                 "id_token_hint=" id-token "&post_logout_redirect_uri=" redirect-uri)
        resp (http/do-get url {:follow-redirects false})]
    (if (= 200 (:status resp))
      :bye
      (do (log/error resp)
          (str "logout failed with status " (:status resp))))))

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
