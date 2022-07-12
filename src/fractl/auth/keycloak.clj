(ns fractl.auth.keycloak
  (:require [keycloak.deployment :as kd]
            [keycloak.user :as ku]
            [fractl.auth.internal :as i]))

(def ^:private tag :keycloak)

;; Return a keycloak client. An is,
;; {:auth-server-url "http://localhost:8090/auth"
;;  :realm "master"
;;  :client-id "admin-cli"
;;  :admin "admin"
;;  :admin-password "secretadmin"}
(defmethod i/make-client tag [config]
  (let [admin (:admin config)
        pswd (:admin-password config)]
    (-> (kd/client-conf (dissoc config :service :admin :admin-password))
        (kd/keycloak-client admin pswd))))

(defn- user-properties [inst]
  {:username (:Name inst)
   :first-name (:FirstName inst)
   :last-name (:LastName inst)
   :password (:Password inst)
   :email (:Email inst)})

(defmethod i/upsert-user tag [{kc-client i/client-key
                               realm :realm
                               inst i/instance-key}]
  (let [obj (user-properties inst)]
    (ku/create-or-update-user! kc-client realm obj nil nil)
    inst))

(defmethod i/delete-user tag [{kc-client i/client-key
                               realm :realm
                               inst i/instance-key}]
  (ku/delete-user! kc-client realm (:Name inst))
  inst)
