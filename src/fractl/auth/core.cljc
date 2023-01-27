(ns fractl.auth.core
  (:require [fractl.auth.model]))

(def service-tag :service)

(defmulti make-client service-tag)
;; Returns the function: `(fn [req token] (verify-and-extract token))`
;; The returned function is called every time an API request with Bearer token is received.
(defmulti make-authfn service-tag)
(defmulti user-login service-tag)
;; Return user details that you want to attach in EventContext. 
;; Include `username` and `sub`.
(defmulti session-user service-tag)
;; Can be same as `session-user`.
(defmulti session-sub service-tag)
;; Logout from the auth broker and return some truthy value.
(defmulti user-logout service-tag)
;; Get user details
(defmulti get-user service-tag)
(defmulti upsert-user service-tag)
(defmulti delete-user service-tag)
(defmulti forgot-password service-tag)
(defmulti confirm-forgot-password service-tag)
(defmulti change-password service-tag)
(defmulti refresh-token service-tag)

(def client-key :client)
(def instance-key :instance)

(defn call-upsert-user [client arg user-inst]
  (upsert-user (assoc arg client-key client instance-key user-inst)))

(defn call-delete-user [client arg user-inst]
  (delete-user (assoc arg client-key client instance-key user-inst)))
