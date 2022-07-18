(ns fractl.util.auth
  (:require [fractl.component :as cn]
            [fractl.lang.internal :as li]))

(def login-event-name :Kernel.Identity/UserLogin)
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
