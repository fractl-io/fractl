(ns fractl.evaluator.internal
  (:require [fractl.lang.opcode :as opc]))

(def dispatch-table opc/dispatch-table)

;; Result builders.
(defn make-result
  ([status result env message]
   {opc/result-status-tag status :result result
    :env env :message message})
  ([status result env] (make-result status result env nil))
  ([status] {opc/result-status-tag status}))

;; result builders
(def ok (partial make-result opc/ok-tag))

(def not-found (partial make-result opc/not-found-tag))
(def declined (partial make-result opc/declined-tag))

(defn error [message]
  (make-result opc/error-tag nil nil message))

(defn- tag-eq? [tag r]
  (= tag (opc/result-status r)))

(def ok? (partial tag-eq? opc/ok-tag))
(def error? (partial tag-eq? opc/error-tag))

(defn dummy-result [env]
  {opc/result-status-tag opc/ok-tag :env env})
