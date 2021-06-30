(ns fractl.policy.rbac
  (:require [fractl.component :as cn]
            [fractl.resolver.policy :as rp]))

(defn evaluate?
  "Return true if the event-instance meet
  rules set for pre-eval rbac, if not return
  false."
  [event-instance zero-trust-rbac]
  (if-let [rules (rp/rbac-eval-rules
                  (cn/instance-name event-instance))]
    (every? #(% event-instance) rules)
    ;; if no rules are set, allow the evaluation.
    (not zero-trust-rbac)))

(defn install-entity-policies [event-name spec]
  (doseq [[n opr] spec]
    (when (cn/find-entity-schema n)
      (doseq [rule (rp/rbac-eval-rules n)]
        (when (some #{opr} (first rule))
          (rp/policy-upsert
           (cn/make-instance
            :Kernel/Policy
            {:Intercept "RBAC"
             :Resource [event-name]
             :Rule (second rule)
             :InterceptStage "Default"}))))))
  event-name)
