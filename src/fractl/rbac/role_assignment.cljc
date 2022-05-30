(ns fractl.rbac.role-assignment
  "Policy management"
  (:require [fractl.lang :as f]
            [fractl.evaluator :as ev]))

(f/component :Kernel.Role)

(f/entity
 :Kernel.Role/Assignment
 {:Role {:type :Kernel/String :indexed true}
  :AssigneeId {:type :Kernel/UUID :indexed true}
  :AssigneeType :Kernel/Path})

(f/dataflow
 :Kernel/RoleAssignment
 {:Kernel.Role/Assignment
  {:Role :Kernel/RoleAssignment.Role.Name
   :AssigneeId :Kernel/RoleAssignment.Assignee.Id
   :AssigneeType '(fractl.component/instance-type :Kernel/RoleAssignment.Assignee)}})

(f/dataflow
 :Kernel.Role/FindAssignedRoles
 {:Kernel.Role/Assignment
  {:AssigneeId? :Kernel.Role/FindAssignedRoles.AssigneeId}
  :as :A}
 [:for-each :A '(:Role :%)])

(defn find-assigned-roles [assignee-id]
  (let [result (ev/eval-all-dataflows
                {:Kernel.Role/FindAssignedRoles
                 {:AssigneeId assignee-id}})]
    (ev/ok-result result)))
