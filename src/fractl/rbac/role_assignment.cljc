(ns fractl.rbac.role-assignment
  "Policy management"
  (:require [fractl.lang :as f]
            [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]))

(f/component :Kernel.Role)

(f/entity
 :Kernel.Role/Assignment
 {:Role {:type :Kernel/String :indexed true}
  :AssigneeId {:type :Kernel/UUID :indexed true}
  :AssigneeType :Kernel/Path})

(f/dataflow
 :Kernel.Role/AssignedRoles
 {:Kernel.Role/Assignment
  {:AssigneeId? :Kernel.Role/AssignedRoles.AssigneeId}})

(defn find-assigned-roles [assignee-id]
  (let [evt (cn/make-instance
             {:Kernel.Role/AssignedRoles
              {:AssigneeId assignee-id}})
        result (ev/eval-all-dataflows evt)]
    (when-let [r (ev/ok-result result)]
      (mapv :Role r))))
