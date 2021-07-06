(ns fractl.policy.rbac
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.resolver.policy :as rp]))

(defn evaluate-dataflow?
  "Return true if the event-instance meet
  rules set for pre-eval rbac, if not return
  false."
  [event-instance zero-trust-rbac]
  (if-let [rules (rp/rbac-eval-rules
                  (cn/instance-name event-instance))]
    (every? #(% event-instance) rules)
    ;; if no rules are set, allow the evaluation.
    (not zero-trust-rbac)))

(defn evaluate-opcode?
  "Return true if it is permitted to perform the specified
  CRUD action on an entity."
  [event-instance zero-trust-rbac action rec-name caller-data]
  (if (cn/find-entity-schema rec-name)
    (let [evt (assoc-in event-instance [:EventContext :Data] caller-data)]
      (if-let [rules (seq (rp/rbac-eval-rules rec-name))]
        (loop [rules rules]
          (if-let [rule (first rules)]
            (if (some #{action} (first rule))
              (when ((second rule) evt)
                (recur (rest rules)))
              (recur (rest rules)))
            true))
        (not zero-trust-rbac)))
    true))
