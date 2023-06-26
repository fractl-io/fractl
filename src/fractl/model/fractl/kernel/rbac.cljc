(ns
 fractl.model.fractl.kernel.rbac
 (:require [clojure.string :as s] [fractl.store.util :as stu])
 (:use
  [fractl.model.fractl.kernel.lang
   :only
   [Fractl_Kernel_Lang___COMPONENT_ID__]]
  [fractl.lang
   :only
   [dataflow entity attribute relationship component event record]]))
(component
 :Fractl.Kernel.Rbac
 {:refer [:Fractl.Kernel.Lang],
  :clj-import
  '[(:require [clojure.string :as s] [fractl.store.util :as stu])]})
(entity
 :Fractl.Kernel.Rbac/Role
 {:Name {:type :String, :indexed true, :unique true}})
(def oprs #{:read :create :update :delete :eval})
(defn-
 crud-list?
 [xs]
 (every? (fn* [p1__264#] (some #{p1__264#} oprs)) (set xs)))
(entity
 :Fractl.Kernel.Rbac/Privilege
 {:Name {:type :String, :indexed true, :unique true},
  :Actions {:check fractl.model.fractl.kernel.rbac/crud-list?},
  :Resource :Edn})
(entity
 :Fractl.Kernel.Rbac/PrivilegeAssignment
 {:Role {:ref :Fractl.Kernel.Rbac/Role.Name, :indexed true},
  :Privilege {:ref :Fractl.Kernel.Rbac/Privilege.Name},
  :meta
  {:unique [:Fractl.Kernel.Rbac/Role :Fractl.Kernel.Rbac/Privilege]}})
(entity
 :Fractl.Kernel.Rbac/RoleAssignment
 {:Role {:ref :Fractl.Kernel.Rbac/Role.Name, :indexed true},
  :Assignee {:type :String, :indexed true},
  :meta {:unique [:Fractl.Kernel.Rbac/Role :Assignee]}})
(entity
 :Fractl.Kernel.Rbac/RoleRelationship
 {:Parent {:ref :Fractl.Kernel.Rbac/Role.Name, :indexed true},
  :Child {:ref :Fractl.Kernel.Rbac/Role.Name, :indexed true},
  :meta {:unique [:Parent :Child]}})
(event
 :Fractl.Kernel.Rbac/AssignRelationship
 {:Parent :String, :Child :String})
(dataflow
 :Fractl.Kernel.Rbac/AssignRelationship
 #:Fractl.Kernel.Rbac{:RoleRelationship
                      {:Parent
                       :Fractl.Kernel.Rbac/AssignRelationship.Parent,
                       :Child
                       :Fractl.Kernel.Rbac/AssignRelationship.Child}})
(event :Fractl.Kernel.Rbac/FindChildren {:Parent :String})
(dataflow
 :Fractl.Kernel.Rbac/FindChildren
 [:for-each
  #:Fractl.Kernel.Rbac{:RoleRelationship
                       {:Parent?
                        :Fractl.Kernel.Rbac/FindChildren.Parent}}
  #:Fractl.Kernel.Rbac{:Role
                       {:Name?
                        :Fractl.Kernel.Rbac/RoleRelationship.Child}}])
(dataflow
 :Fractl.Kernel.Rbac/FindRoleAssignments
 #:Fractl.Kernel.Rbac{:RoleAssignment
                      {:Assignee?
                       :Fractl.Kernel.Rbac/FindRoleAssignments.Assignee}})
(defn-
 priv-assigns-query
 [env]
 (let
  [role-names
   (env :Fractl.Kernel.Rbac/FindPrivilegeAssignments.RoleNames)]
  (str
   "SELECT * FROM "
   (stu/entity-table-name :Fractl.Kernel.Rbac/PrivilegeAssignment)
   " WHERE ("
   (stu/attribute-column-name :Role)
   " in ("
   (s/join
    ","
    (map (fn* [p1__265#] (str "'" (str p1__265#) "'")) role-names))
   "))")))
(dataflow
 :Fractl.Kernel.Rbac/FindPrivilegeAssignments
 [:query
  #:Fractl.Kernel.Rbac{:PrivilegeAssignment?
                       fractl.model.fractl.kernel.rbac/priv-assigns-query}])
(defn-
 privileges-query
 [env]
 (let
  [names (env :Fractl.Kernel.Rbac/FindPrivileges.Names)]
  (str
   "SELECT * FROM "
   (stu/entity-table-name :Fractl.Kernel.Rbac/Privilege)
   " WHERE ("
   (stu/attribute-column-name :Name)
   " in ("
   (s/join
    ","
    (map (fn* [p1__266#] (str "'" (str p1__266#) "'")) names))
   "))")))
(dataflow
 :Fractl.Kernel.Rbac/FindPrivileges
 [:query
  #:Fractl.Kernel.Rbac{:Privilege?
                       fractl.model.fractl.kernel.rbac/privileges-query}])
(entity
 :Fractl.Kernel.Rbac/InstancePrivilegeAssignment
 {:Id :Identity,
  :Actions {:check fractl.model.fractl.kernel.rbac/crud-list?},
  :Filter
  {:check fractl.model.fractl.kernel.rbac/crud-list?, :optional true},
  :Resource {:type :Path, :indexed true},
  :ResourceId {:type :Any, :indexed true},
  :Assignee {:type :String, :indexed true},
  :meta {:unique [:Assignee :Resource :ResourceId]}})
(dataflow
 :Fractl.Kernel.Rbac/FindInstancePrivileges
 #:Fractl.Kernel.Rbac{:InstancePrivilegeAssignment
                      {:Resource?
                       :Fractl.Kernel.Rbac/FindInstancePrivileges.Resource,
                       :ResourceId?
                       :Fractl.Kernel.Rbac/FindInstancePrivileges.ResourceId}})
(dataflow
 :Fractl.Kernel.Rbac/DeleteInstancePrivileges
 {:Fractl.Kernel.Rbac/InstancePrivilegeAssignment
  {:Resource? :Fractl.Kernel.Rbac/DeleteInstancePrivileges.Resource,
   :ResourceId?
   :Fractl.Kernel.Rbac/DeleteInstancePrivileges.ResourceId},
  :as :Privs}
 [:for-each
  :Privs
  [:delete
   :Fractl.Kernel.Rbac/InstancePrivilegeAssignment
   {:Id :%.Id}]])
(def
 Fractl_Kernel_Rbac___COMPONENT_ID__
 "1f6423dc-8730-4af9-868b-47f9c32107e8")
