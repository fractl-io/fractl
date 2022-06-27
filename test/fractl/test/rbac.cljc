(ns fractl.test.rbac
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [fractl.rbac.core :as rbac]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.evaluator.intercept :as ei]
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
  (let [[r1 r2 r3 r4]
        (mapv tu/result [:RoleMgmt/CreateUsers :RoleMgmt/CreateRoles
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

(defn- with-user [user-name event]
  (cn/assoc-event-context-user
   user-name
   (cn/make-instance
    (if (keyword? event)
      {event {}}
      event))))

(deftest rbac-application
  (defcomponent :PrivTest
    (entity
     :PrivTest/User
     {:User {:ref :Kernel.RBAC/User.Name}
      :Password :Kernel/Password})
    (entity
     :PrivTest/E
     {:X :Kernel/Int})
    (dataflow
     :PrivTest/CreateSuperUser
     {:PrivTest/User
      {:User "superuser"
       :Password "xyz123"}})
    (dataflow
     :PrivTest/CreateUsers
     {:Kernel.RBAC/User
      {:Name "u11"}}
     {:Kernel.RBAC/User
      {:Name "u22"}}
     {:PrivTest/User
      {:User "u11"
       :Password "kkklll"}}
     {:PrivTest/User
      {:User "u22"
       :Password "yyyduud"}})
    (dataflow
     :PrivTest/CreatePrivileges
     {:Kernel.RBAC/Role {:Name "r11"}}
     {:Kernel.RBAC/Role {:Name "r22"}}
     {:Kernel.RBAC/Privilege
      {:Name "p11"
       :Actions [:q# [:read :upsert]]
       :Resource [:q# [:PrivTest/E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p22"
       :Actions [:q# [:eval]]
       :Resource [:q# [:PrivTest/Upsert_E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p33"
       :Actions [:q# [:read]]
       :Resource [:q# [:PrivTest/E]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r11" :Privilege "p11"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r11" :Privilege "p22"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r22" :Privilege "p33"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r11" :Assignee "u11"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r22" :Assignee "u22"}}))
  (is (rbac/init))
  (let [su (first (tu/result :PrivTest/CreateSuperUser))]
    (is (cn/instance-of? :PrivTest/User su))
    (is (= "superuser" (:User su)))
    (is (= [:rbac] (ei/init-interceptors [:rbac])))
    (let [u2 (first
              (tu/result
               (with-user "superuser" :PrivTest/CreateUsers)))]
      (is (cn/instance-of? :PrivTest/User u2))
      (is (= "u22" (:User u2))))
    (let [r2 (first
              (tu/result
               (with-user "superuser" :PrivTest/CreatePrivileges)))]
      (is (cn/instance-of? :Kernel.RBAC/RoleAssignment r2))
      (is (and (= "r22" (:Role r2)) (= "u22" (:Assignee r2)))))
    (tu/is-error
     #(ev/eval-all-dataflows
       (cn/make-instance
        {:PrivTest/Upsert_E
         {:Instance
          {:PrivTest/E
           {:X 100}}}})))
    (tu/is-error
     #(ev/eval-all-dataflows
       (with-user
         "u22"
         {:PrivTest/Upsert_E
          {:Instance
           {:PrivTest/E
            {:X 200}}}})))
    (is (cn/instance-of?
         :PrivTest/E
         (first
          (tu/result
           (with-user
             "u11"
             {:PrivTest/Upsert_E
              {:Instance
               {:PrivTest/E
                {:X 100}}}})))))
    (ei/reset-interceptors!)))
