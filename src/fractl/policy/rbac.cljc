(ns fractl.policy.rbac
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.resolver.policy :as rp]))

(def ^:private rbac-inited (u/make-cell false))

(defn init! []
  (u/safe-set rbac-inited true))

(defn evaluate-dataflow?
  "Return true if the event-instance meet
  rules set for pre-eval rbac, if not return
  false."
  [event-instance zero-trust-rbac]
  (if @rbac-inited
    (if-let [rules (rp/rbac-rules
                    (cn/instance-name event-instance))]
      (every? #(% event-instance) rules)
      ;; if no rules are set, allow the evaluation.
      (not zero-trust-rbac))
    true))

(defn evaluate-opcode?
  "Return true if it is permitted to perform the specified
  CRUD action on an entity."
  [event-instance zero-trust-rbac action rec-name caller-data]
  (if (and @rbac-inited (cn/find-entity-schema rec-name))
    (let [evt (assoc-in event-instance [li/event-context :Data] caller-data)]
      (if-let [rules (seq (rp/rbac-rules rec-name))]
        (loop [rules rules, result false]
          (if-let [rule (first rules)]
            (if (some #{action} (first rule))
              (when ((second rule) evt)
                (recur (rest rules) true))
              (recur (rest rules) result))
            result))
        (not zero-trust-rbac)))
    true))
