(ns fractl.auth.keycloak
  (:require [keycloak.deployment :as kd]
            [keycloak.backend :as kb]
            [keycloak.user :as ku]
            [keycloak.authn :as ka]
            [org.httpkit.client :as http]
            [fractl.component :as cn]
            [fractl.datafmt.json :as json]
            [fractl.util :as u]
            [fractl.util.auth :as au]
            [fractl.auth.jwt :as jwt]
            [fractl.auth.core :as auth]))

(def ^:private tag :keycloak)
(def ^:private non-client-keys [:user-realm :user-client-id
                                :sub :service :admin :admin-password
                                :auth-user-type])

(def ^:private client (atom nil))

;; Return a keycloak client. An is,
;; {:auth-server-url "http://localhost:8090/auth"
;;  :realm "master"
;;  :user-realm "fractl-dev"
;;  :client-id "admin-cli"
;;  :admin "admin"
;;  :admin-password "secretadmin"}
(defmethod auth/make-client tag [config]
  (or @client
      (let [c (let [admin (:admin config)
                    pswd (:admin-password config)]
                (-> (kd/client-conf (apply dissoc config non-client-keys))
                    (kd/keycloak-client admin pswd)))]
        (reset! client c)
        c)))

(defn- config-as-client-conf [config]
  {:auth-server-url (:auth-server-url config)
   :admin-realm (:realm config)
   :realm (:user-realm config)
   :admin-username (:admin config)
   :admin-password (:admin-password config)
   :client-id (or (:user-client-id config)
                  (:user-realm config))})

(defmethod auth/make-authfn tag [config]
  (kb/buddy-verify-token-fn
   (kd/deployment
    (kd/client-conf
     (config-as-client-conf config)))))

(defn oidc-connect-url [auth-server-url realm-name]
  (str auth-server-url "/realms/" realm-name "/protocol/openid-connect/token"))

(defn client-credentials [client-id username password]
  {:grant_type "password"
   :client_id client-id
   :username username
   :password password
   :scope "openid"})

(defn authenticate [auth-server-url realm client-id username password]
  (let [r @(http/post
            (oidc-connect-url auth-server-url realm)
            {:form-params (client-credentials client-id username password)
             :headers {"Content-Type" "application/x-www-form-urlencoded"}})]
    {:status (:status r)
     :body (json/decode (:body r))}))

(defmethod auth/user-login tag [{url :auth-server-url
                                 realm :user-realm
                                 client-id :user-client-id
                                 event-inst :event}]
  (if-let [obj (au/as-login-event event-inst)]
    (authenticate
     url realm client-id
     (au/login-username obj) (au/login-password obj))
    (u/throw-ex (str "failed to convert to login event: " (cn/instance-type event-inst)))))

(defn- user-properties [inst]
  {:username (:Email inst)
   :first-name (:FirstName inst)
   :last-name (:LastName inst)
   :password (:Password inst)
   :email (:Email inst)})

(defmethod auth/upsert-user tag [{kc-client auth/client-key
                                  realm :user-realm
                                  inst auth/instance-key :as arg}]
  (let [obj (user-properties inst)]
    (ku/create-or-update-user!
     (or kc-client (auth/make-client arg))
     realm obj nil nil)
    inst))

(defmethod auth/delete-user tag [{kc-client auth/client-key
                                  realm :user-realm
                                  inst auth/instance-key :as arg}]
  (ku/delete-user! (or kc-client (auth/make-client arg)) realm (:Name inst))
  inst)

(defmethod auth/user-logout tag [{realm :user-realm
                                  sub :sub :as arg}]
  (let [kc-client (auth/make-client arg)]
    (ku/logout-user! kc-client realm sub)
    :bye))

(defn- get-session-value [request k]
  (let [token (au/bearer-token request)]
    (k (jwt/decode token))))

(defmethod auth/session-user tag [{request :request}]
  (get-session-value request :preferred_username))

(defmethod auth/session-sub tag [{request :request}]
  (get-session-value request :sub))
