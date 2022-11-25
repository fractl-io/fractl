(ns fractl.auth.core
  (:require [fractl.auth.model]))

(def service-tag :service)

(defmulti make-client service-tag)
(defmulti make-authfn service-tag)
(defmulti user-login service-tag)
;; related to session-sub
(defmulti session-user service-tag)
;; token contains info about user. it should send the username. 
;; returns username as string.
;; add github-config from `component/assoc-event-context-value`
;; session-sub might return a qualified user
;; sub means subject here
(defmulti session-sub service-tag)
;; should return some truthy value
(defmulti user-logout service-tag)
;; don't implement (throw exception: not implelemented)
(defmulti upsert-user service-tag)
;; don't implement (throw exception: not implelemented)
(defmulti delete-user service-tag)

(def client-key :client)
(def instance-key :instance)

(defn call-upsert-user [client arg user-inst]
  (upsert-user (assoc arg client-key client instance-key user-inst)))

(defn call-delete-user [client arg user-inst]
  (delete-user (assoc arg client-key client instance-key user-inst)))
