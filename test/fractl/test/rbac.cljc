(ns fractl.test.rbac
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [fractl.rbac.core :as rbac]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            #?(:clj  [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest role-management
  (defcomponent :RoleMgmt
    (dataflow
     :RoleMgmt/CreateUsers
     {:Kernel.RBAC/User {:Name "abc"}}
     {:Kernel.RBAC/User {:Name "xyz"}})
    (dataflow
     :RoleMgmt/CreateRoles
     {:Kernel.RBAC/Role {:Name "role1"}}
     {:Kernel.RBAC/Role {:Name "role2"}})
    (dataflow
     :RoleMgmt/AssignPrivileges
     {:Kernel.RBAC/Privilege
      {:Name "p1"
       :Actions [:q# [:read :upsert]]
       :Resource [:q# [:A :B]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p2"
       :Actions [:q# [:read]]
       :Resource [:q# [:C]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r1" :Privilege "p1"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r1" :Privilege "p2"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r2" :Privilege "p2"}})
    (dataflow
     :RoleMgmt/AssignRoles
     {:Kernel.RBAC/RoleAssignment
      {:Role "r1" :Assignee "abc"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r2" :Assignee "xyz"}}))
  (let [[r1 r2 r3 r4] (mapv
                       #(tu/result {% {}})
                       [:RoleMgmt/CreateUsers :RoleMgmt/CreateRoles
                        :RoleMgmt/AssignPrivileges :RoleMgmt/AssignRoles])]
    (is (cn/instance-of? :Kernel.RBAC/User (first r1)))
    (is (cn/instance-of? :Kernel.RBAC/Role (first r2)))
    (is (cn/instance-of? :Kernel.RBAC/PrivilegeAssignment (first r3)))
    (is (cn/instance-of? :Kernel.RBAC/RoleAssignment (first r4)))
    (let [ps1 (rbac/privileges "abc")
          ps2 (rbac/privileges "xyz")
          p2 (first ps2)]
      (is (= (count ps1) 2))
      (is (= (count ps2) 1))
      (is (= [:read] (:Actions p2)))
      (is (= [:C] (:Resource p2))))))
