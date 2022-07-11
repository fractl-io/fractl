(ns fractl.rbac.model
  (:require [clojure.string :as s]
            [fractl.lang
             :refer [component event entity dataflow]]
            [fractl.store.util :as stu]
            [fractl.evaluator :as ev]))

(component :Kernel.RBAC)

(entity
 :Kernel.RBAC/User
 {:Name {:type :Kernel/String
         :indexed true
         :unique true}
  :Password {:type :Kernel/Password
             :optional true} ; may use social-login
  :FirstName {:type :Kernel/String
              :optional true}
  :LastName {:type :Kernel/String
             :optional true}
  :Email {:type :Kernel/Email
          :optional true}})

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
  :Privilege {:ref :Kernel.RBAC/Privilege.Name}
  :meta {:unique [:Role :Privilege]}})

(entity
 :Kernel.RBAC/RoleAssignment
 {:Role {:ref :Kernel.RBAC/Role.Name
         :indexed true}
  :Assignee {:ref :Kernel.RBAC/User.Name
             :indexed true}
  :meta
  {:unique [:Role :Assignee]}})

(entity
 :Kernel.RBAC/RoleRelationship
 {:Parent {:ref :Kernel.RBAC/Role.Name
           :indexed true}
  :Child {:ref :Kernel.RBAC/Role.Name
          :indexed true}
  :meta
  {:unique [:Parent :Child]}})

(dataflow
 :Kernel.RBAC/FindUser
 {:Kernel.RBAC/User
  {:Name? :Kernel.RBAC/FindUser.Name}})

(dataflow
 :Kernel.RBAC/FindRoleAssignments
 {:Kernel.RBAC/RoleAssignment
  {:Assignee? :Kernel.RBAC/FindRoleAssignments.Assignee}})

(defn- priv-assigns-query [env]
  (let [role-names (env :Kernel.RBAC/FindPrivilegeAssignments.RoleNames)]
    (str "SELECT * FROM " (stu/entity-table-name :Kernel.RBAC/PrivilegeAssignment)
         " WHERE (" (stu/attribute-column-name :Role) " in ("
         (s/join "," (map #(str "'" (str %) "'") role-names)) "))")))

(dataflow
 :Kernel.RBAC/FindPrivilegeAssignments
 [:query {:Kernel.RBAC/PrivilegeAssignment? priv-assigns-query}])

(defn- privileges-query [env]
  (let [names (env :Kernel.RBAC/FindPrivileges.Names)]
    (str "SELECT * FROM " (stu/entity-table-name :Kernel.RBAC/Privilege)
         " WHERE (" (stu/attribute-column-name :Name) " in ("
         (s/join "," (map #(str "'" (str %) "'") names)) "))")))

(dataflow
 :Kernel.RBAC/FindPrivileges
 [:query {:Kernel.RBAC/Privilege? privileges-query}])
