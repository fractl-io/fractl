(ns fractl.auth.keycloak
  (:require [keycloak.deployment :as kd]
            [keycloak.backend :as kb]
            [keycloak.user :as ku]
            [keycloak.authn :as ka]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [fractl.auth.internal :as i]))

(def ^:private tag :keycloak)
(def ^:private non-client-keys [:user-realm :user-client-id :service :admin :admin-password])

;; Return a keycloak client. An is,
;; {:auth-server-url "http://localhost:8090/auth"
;;  :realm "master"
;;  :user-realm "fractl-dev"
;;  :client-id "admin-cli"
;;  :admin "admin"
;;  :admin-password "secretadmin"}
(defmethod i/make-client tag [config]
  (let [admin (:admin config)
        pswd (:admin-password config)]
    (-> (kd/client-conf (apply dissoc config non-client-keys))
        (kd/keycloak-client admin pswd))))

(defn- config-as-client-conf [config]
  {:auth-server-url (:auth-server-url config)
   :admin-realm (:realm config)
   :realm (:user-realm config)
   :admin-username (:admin config)
   :admin-password (:admin-password config)
   :client-id (or (:user-client-id config)
                  (:user-realm config))})

(defmethod i/make-authfn tag [config]
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
    (json/parse-string (:body r) true)))

(defmethod i/user-login tag [{url :auth-server-url
                              realm :user-realm
                              client-id :user-client-id
                              username :username
                              password :password}]
  (authenticate url realm client-id username password))

(defn- user-properties [inst]
  {:username (:Name inst)
   :first-name (:FirstName inst)
   :last-name (:LastName inst)
   :password (:Password inst)
   :email (:Email inst)})

(defmethod i/upsert-user tag [{kc-client i/client-key
                               realm :user-realm
                               inst i/instance-key}]
  (let [obj (user-properties inst)]
    (ku/create-or-update-user! kc-client realm obj nil nil)
    inst))

(defmethod i/delete-user tag [{kc-client i/client-key
                               realm :user-realm
                               inst i/instance-key}]
  (ku/delete-user! kc-client realm (:Name inst))
  inst)
