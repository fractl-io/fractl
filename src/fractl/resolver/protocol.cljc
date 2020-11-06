(ns fractl.resolver.protocol
  (:require [fractl.lang.opcode :as opc]))

(defn make
  ([vm store config]
   {:vm vm
    :store store
    :config config})
  ([vm store] (make vm store nil)))

(def vm :vm)
(def store :store)
(def config :config)

(def dispatch-table opc/dispatch-table)

;; Result builders.
(defn- res
  ([status result env message]
   {:status status :result result
    :env env :message message})
  ([status result env] (res status result env nil))
  ([status] {:status status}))

(def ok (partial res :ok))

(def not-found (res :not-found))
(def declined (res :declined))
(def no-op (res :no-op))

(defn error [message]
  (res :error nil nil message))

(defn ok? [r] (= :ok (:status r)))

(defn dummy-result [env]
  {:status :ok :env env})
