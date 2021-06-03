(ns fractl.policy.rbac
  (:require [fractl.component :as cn]
            [fractl.resolver.policy :as pr]))

(defn evaluate?
  "Return true if the event-instance meet
  rules set for pre-eval rbac, if not return
  false."
  [event-instance]
  (if-let [rules (pr/rbac-eval-rules
                  (cn/instance-name event-instance))]
    (every? #(% event-instance) rules)
    ;; if no rules are set, allow the evaluation.
    true))
