(ns fractl.resolver.protocol
  (:require [fractl.lang.opcode :as opc]))

(def Resolver
  "Protocol for resolution or evaluation of various patterns.
   There is a 1-1 correspondence with this protocol and the
   virtual-machine instructions defined in v8dml.lang.opcode.

   The methods declared below return a map with status information and result.
   The general forms of this map are:
    - {:status :ok :result <optional-result> :env <optional-updated-environment>}, call success.
    - {:status :not-found}, a query or lookup returned no-data.
    - {:status :declined}, the resolver refuses to service the call.
    - {:status :no-op}, no operation needs to be performed, for instance, no dataflows attached to an event.
    - {:status :error :message <error-message>}, there was an unxpected error."
  opc/VM)

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
