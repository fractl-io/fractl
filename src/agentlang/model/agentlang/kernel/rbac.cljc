(ns
 agentlang.model.agentlang.kernel.rbac
 (:require
  [clojure.string :as s]
  [agentlang.util :as u]
  [agentlang.store.util :as stu]
  [agentlang.lang.internal :as li])
 (:use
  [agentlang.model.agentlang.kernel.lang
   :only
   [Agentlang_Kernel_Lang___COMPONENT_ID__]]
  [agentlang.lang
   :only
   [dataflow
    entity
    view
    attribute
    rule
    relationship
    component
    resolver
    event
    inference
    record]]))
(component
 :Agentlang.Kernel.Rbac
 {:refer [:Agentlang.Kernel.Lang],
  :clj-import
  '[(:require
     [clojure.string :as s]
     [agentlang.util :as u]
     [agentlang.store.util :as stu]
     [agentlang.lang.internal :as li])]})
(entity :Agentlang.Kernel.Rbac/Role {:Name {:type :String, li/guid true}})
(def oprs li/rbac-oprs)
(defn-
 crud-list?
 [xs]
 (every? (fn* [p1__288#] (some #{p1__288#} oprs)) (set xs)))
(entity
 :Agentlang.Kernel.Rbac/Privilege
 {:Name {:type :String, :default u/uuid-string, li/guid true},
  :Actions {:check agentlang.model.agentlang.kernel.rbac/crud-list?},
  :Resource :Edn})
(entity
 :Agentlang.Kernel.Rbac/PrivilegeAssignment
 {:Name {:type :String, :default u/uuid-string, li/guid true},
  :Role {:ref :Agentlang.Kernel.Rbac/Role.Name, :indexed true},
  :Privilege {:ref :Agentlang.Kernel.Rbac/Privilege.Name},
  :meta
  {:unique [:Agentlang.Kernel.Rbac/Role :Agentlang.Kernel.Rbac/Privilege]}})
(entity
 :Agentlang.Kernel.Rbac/RoleAssignment
 {:Name {:type :String, :default u/uuid-string, li/guid true},
  :Role {:ref :Agentlang.Kernel.Rbac/Role.Name, :indexed true},
  :Assignee {:type :String, :indexed true},
  :meta {:unique [:Agentlang.Kernel.Rbac/Role :Assignee]}})
(dataflow
 :Agentlang.Kernel.Rbac/FindRoleAssignments
 #:Agentlang.Kernel.Rbac{:RoleAssignment
                      {:Assignee?
                       :Agentlang.Kernel.Rbac/FindRoleAssignments.Assignee}})
(dataflow
 :Agentlang.Kernel.Rbac/DeleteRoleAssignments
 [:delete
  :Agentlang.Kernel.Rbac/RoleAssignment
  {:Assignee :Agentlang.Kernel.Rbac/DeleteRoleAssignments.Assignee}])
(defn-
 priv-assigns-query
 [env]
 (let
  [role-names
   (env :Agentlang.Kernel.Rbac/FindPrivilegeAssignments.RoleNames)]
  (str
   "SELECT * FROM "
   (stu/entity-table-name :Agentlang.Kernel.Rbac/PrivilegeAssignment)
   " WHERE ("
   (stu/attribute-column-name :Role)
   " in ("
   (s/join
    ","
    (map (fn* [p1__289#] (str "'" (str p1__289#) "'")) role-names))
   "))")))
(dataflow
 :Agentlang.Kernel.Rbac/FindPrivilegeAssignments
 [:query
  #:Agentlang.Kernel.Rbac{:PrivilegeAssignment?
                       agentlang.model.agentlang.kernel.rbac/priv-assigns-query}])
(defn-
 privileges-query
 [env]
 (let
  [names (env :Agentlang.Kernel.Rbac/FindPrivileges.Names)]
  (str
   "SELECT * FROM "
   (stu/entity-table-name :Agentlang.Kernel.Rbac/Privilege)
   " WHERE ("
   (stu/attribute-column-name :Name)
   " in ("
   (s/join
    ","
    (map (fn* [p1__290#] (str "'" (str p1__290#) "'")) names))
   "))")))
(dataflow
 :Agentlang.Kernel.Rbac/FindPrivileges
 [:query
  #:Agentlang.Kernel.Rbac{:Privilege?
                       agentlang.model.agentlang.kernel.rbac/privileges-query}])
(entity
 #:Agentlang.Kernel.Rbac{:InstancePrivilegeAssignment
                      {:Name
                       {:type :String,
                        :default u/uuid-string,
                        li/guid true},
                       :Actions
                       {:check
                        agentlang.model.agentlang.kernel.rbac/crud-list?,
                        :optional true},
                       :Resource :Path,
                       :ResourceId :Any,
                       :Assignee {:type :String, :indexed true}}})
(entity
 #:Agentlang.Kernel.Rbac{:OwnershipAssignment
                      {:Name
                       {:type :String,
                        :default u/uuid-string,
                        li/guid true},
                       :Resource :Path,
                       :ResourceId :Any,
                       :Assignee {:type :String, :indexed true}}})
(def
 Agentlang_Kernel_Rbac___COMPONENT_ID__
 "62efa96d-e60e-48b4-98aa-e4d0560fade9")
