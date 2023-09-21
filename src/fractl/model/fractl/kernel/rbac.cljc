(ns
 fractl.model.fractl.kernel.rbac
 (:require
  [clojure.string :as s]
  [fractl.util :as u]
  [fractl.store.util :as stu]
  [fractl.lang.internal :as li])
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
  '[(:require
     [clojure.string :as s]
     [fractl.util :as u]
     [fractl.store.util :as stu]
     [fractl.lang.internal :as li])]})
(entity
 :Fractl.Kernel.Rbac/Role
 {:Name {:type :String, :indexed true, :unique true}})
(def oprs li/rbac-oprs)
(defn-
 crud-list?
 [xs]
 (every? (fn* [p1__277#] (some #{p1__277#} oprs)) (set xs)))
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
    (map (fn* [p1__278#] (str "'" (str p1__278#) "'")) role-names))
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
    (map (fn* [p1__279#] (str "'" (str p1__279#) "'")) names))
   "))")))
(dataflow
 :Fractl.Kernel.Rbac/FindPrivileges
 [:query
  #:Fractl.Kernel.Rbac{:Privilege?
                       fractl.model.fractl.kernel.rbac/privileges-query}])
(entity
 #:Fractl.Kernel.Rbac{:InstancePrivilegeAssignment
                      {:Actions
                       {:check
                        fractl.model.fractl.kernel.rbac/crud-list?,
                        :optional true},
                       :Resource :Path,
                       :ResourceId :Any,
                       :Assignee :String}})
(entity
 #:Fractl.Kernel.Rbac{:OwnershipAssignment
                      {:Resource :Path,
                       :ResourceId :Any,
                       :Assignee :String}})
(def
 Fractl_Kernel_Rbac___COMPONENT_ID__
 "18ad958f-be7e-43cf-8214-6fa1e717a7d2")
