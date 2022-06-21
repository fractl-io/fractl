(ns fractl.rbac.model
  (:require [fractl.lang
             :refer [component event entity]]))

(component :Kernel.RBAC)

(entity
 :Kernel.RBAC/User
 {:Name {:type :Kernel/String
         :indexed true
         :unique true}})

(entity
 :Kernel.RBAC/Role
 {:Name {:type :Kernel/String
         :indexed true
         :unique true}})

(def ^:private oprs #{:read :upsert :delete :eval})

(defn- crud-list? [xs]
  (every? #(some #{%} oprs) (set xs)))

(entity
 :Kernel.RBAC/Privilege
 {:Name {:type :Kernel/String
         :indexed true
         :unique true}
  :Actions {:check crud-list?}
  :Resource :Kernel/Edn})

(entity
 :Kernel.RBAC/PrivilegeAssignment
 {:Role {:ref :Kernel.RBAC/Role.Name
         :indexed true}
  :Privilege {:ref :Kernel.RBAC/Privilege.Name}})

(entity
 :Kernel.RBAC/RoleAssignment
 {:Role {:ref :Kernel.RBAC/Role.Name
         :indexed true}
  :Assignee {:ref :Kernel.RBAC/User.Name
             :indexed true
             :unique true}})

(entity
 :Kernel.RBAC/RoleRelationship
 {:Parent {:ref :Kernel.RBAC/Role.Name
           :indexed true}
  :Child {:ref :Kernel.RBAC/Role.Name
          :indexed true
          :unique true}})
