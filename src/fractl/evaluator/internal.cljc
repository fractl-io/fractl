(ns fractl.evaluator.internal
  (:require [fractl.lang.opcode :as opc]))

(def dispatch-table opc/dispatch-table)

;; Result builders.
(defn make-result
  ([status result env message]
   {:status status :result result
    :env env :message message})
  ([status result env] (make-result status result env nil))
  ([status] {:status status}))

;; result builders
(def ok (partial make-result :ok))

(def not-found (make-result :not-found))
(def declined (make-result :declined))
(def no-op (make-result :no-op))

(defn error [message]
  (make-result :error nil nil message))

(defn ok? [r] (= :ok (:status r)))

(defn dummy-result [env]
  {:status :ok :env env})
