(ns fractl.rbac.role-assignment
  "Policy management"
  (:require [fractl.lang :as f]
            [fractl.component :as cn]))

(f/component :Kernel.Role)

(f/entity
 :Kernel.Role/Assignment
 {:Role {:type :Kernel/String :indexed true}
  :AssigneeId {:type :Kernel/UUID :indexed true}
  :AssigneeType :Kernel/Path})

(f/dataflow
 :Kernel/RoleAssignment
 {:Kernel.Role/Assignment
  {:Role :Kernel/RoleAssignment.Role
   :AssigneeId :Kernel/RoleAssignment.Assignee.Id
   :AssigneeType '(cn/instance-type :Kernel/RoleAssignment.Assignee)}})

(f/dataflow
 :Kernel.Role/FindAssignedRoles
 {:Kernel.Role/Assignment
  {:AssigneeId? :Kernel.Role/FindAssignedRoles.AssigneeId}
  :as :A}
 [:for-each :A '(:Role :%)])
