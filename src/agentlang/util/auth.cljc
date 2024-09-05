(ns agentlang.util.auth
  (:require [clojure.string :as s]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]))

(def login-event-name :Agentlang.Kernel.Identity/UserLogin)
(def login-username :Username)
(def login-password :Password)

(defn make-login-event [username password]
  (cn/make-instance
   login-event-name
   {login-username username
    login-password password}))

(def parsed-login-event-name (li/split-path login-event-name))

(defn login-event-instance? [inst]
  (= parsed-login-event-name
     (li/split-path (cn/instance-type inst))))

(defn as-login-event [generic-event-inst]
  (if (login-event-instance? generic-event-inst)
    generic-event-inst
    (when-let [ord (cn/display-order (cn/instance-type generic-event-inst))]
      (make-login-event ((first ord) generic-event-inst) ((second ord) generic-event-inst)))))

(def ^:private space-pat #" ")

(defn bearer-token [request]
  (when-let [auths (get-in request [:headers "authorization"])]
    (second (s/split auths space-pat))))
