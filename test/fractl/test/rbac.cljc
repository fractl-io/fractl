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
     {:Kernel.Identity/User {:Name "abc"}}
     {:Kernel.Identity/User {:Name "xyz"}})
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
    (is (cn/instance-of? :Kernel.Identity/User (first r1)))
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

(defn- call-with-rbac [f finalize]
  (is (rbac/init))
  (try
    (f)
    (finally
      (finalize))))

(defn- rbac-application []
  (defcomponent :PrivTest
    (entity
     :PrivTest/User
     {:User {:ref :Kernel.Identity/User.Name}})
    (entity
     :PrivTest/E
     {:X :Kernel/Int})
    (dataflow
     :PrivTest/CreateSuperUser
     {:PrivTest/User
      {:User rbac/default-superuser-name}})
    (dataflow
     :PrivTest/CreateUsers
     {:Kernel.Identity/User
      {:Name "u11"}}
     {:Kernel.Identity/User
      {:Name "u22"}}
     {:PrivTest/User
      {:User "u11"}}
     {:PrivTest/User
      {:User "u22"}})
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
     {:Kernel.RBAC/Privilege
      {:Name "p44"
       :Actions [:q# [:eval]]
       :Resource [:q# [:PrivTest/Lookup_E]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r11" :Privilege "p11"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r11" :Privilege "p22"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r22" :Privilege "p33"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r22" :Privilege "p44"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r11" :Assignee "u11"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r22" :Assignee "u22"}}))
  (call-with-rbac
   (fn []
     (let [su (first (tu/result :PrivTest/CreateSuperUser))]
       (is (cn/instance-of? :PrivTest/User su))
       (is (= rbac/default-superuser-name (:User su)))
       (is (= [:rbac] (ei/init-interceptors [:rbac])))
       (let [u2 (first
                 (tu/result
                  (with-user rbac/default-superuser-name :PrivTest/CreateUsers)))]
         (is (cn/instance-of? :PrivTest/User u2))
         (is (= "u22" (:User u2))))
       (let [r2 (first
                 (tu/result
                  (with-user rbac/default-superuser-name :PrivTest/CreatePrivileges)))]
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
       (let [inst (first
                   (tu/result
                    (with-user
                      "u11"
                      {:PrivTest/Upsert_E
                       {:Instance
                        {:PrivTest/E
                         {:X 100}}}})))
             id (cn/id-attr inst)
             lookup {:PrivTest/Lookup_E
                     {cn/id-attr id}}]
         (is (cn/instance-of? :PrivTest/E inst))
         (tu/is-error
          #(ev/eval-all-dataflows
            (with-user "u11" lookup)))
         (let [inst2 (first
                      (tu/result
                       (with-user "u22" lookup)))]
           (cn/same-instance? inst inst2)))))
   #(ei/reset-interceptors!)))

(defn- rbac-with-owner []
  (is (= [:instance-meta] (ei/init-interceptors [:instance-meta])))
  (defcomponent :RbacOwner
    (entity
     :RbacOwner/User
     {:User {:ref :Kernel.Identity/User.Name}})
    (entity
     :RbacOwner/E
     {:X :Kernel/Int})
    (dataflow
     :RbacOwner/CreateSuperUser
     {:RbacOwner/User
      {:User rbac/default-superuser-name}})
    (dataflow
     :RbacOwner/CreateUsers
     {:Kernel.Identity/User
      {:Name "uu11"}}
     {:Kernel.Identity/User
      {:Name "uu22"}}
     {:RbacOwner/User
      {:User "uu11"}}
     {:RbacOwner/User
      {:User "uu22"}})
    (dataflow
     :RbacOwner/CreatePrivileges
     {:Kernel.RBAC/Role {:Name "rr11"}}
     {:Kernel.RBAC/Privilege
      {:Name "pp11"
       :Actions [:q# [:read :upsert]]
       :Resource [:q# [:RbacOwner/E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "pp22"
       :Actions [:q# [:eval]]
       :Resource [:q# [:RbacOwner/Upsert_E
                       :RbacOwner/Lookup_E
                       :RbacOwner/Delete_E]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "rr11" :Privilege "pp11"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "rr11" :Privilege "pp22"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "rr11" :Assignee "uu11"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "rr11" :Assignee "uu22"}}))
  (call-with-rbac
   (fn []
     (let [su (first (tu/result :RbacOwner/CreateSuperUser))]
       (is (cn/instance-of? :RbacOwner/User su))
       (is (= rbac/default-superuser-name (:User su)))
       (is (= [:rbac] (ei/init-interceptors [:rbac])))
       (let [u2 (first
                 (tu/result
                  (with-user rbac/default-superuser-name :RbacOwner/CreateUsers)))]
         (is (cn/instance-of? :RbacOwner/User u2))
         (is (= "uu22" (:User u2))))
       (let [r1 (first
                 (tu/result
                  (with-user rbac/default-superuser-name :RbacOwner/CreatePrivileges)))]
         (is (cn/instance-of? :Kernel.RBAC/RoleAssignment r1))
         (is (and (= "rr11" (:Role r1)) (= "uu22" (:Assignee r1)))))
       (let [e1 (first
                 (tu/result
                  (with-user
                    "uu11"
                    {:RbacOwner/Upsert_E
                     {:Instance
                      {:RbacOwner/E
                       {:X 100}}}})))
             e2 (first
                 (tu/result
                  (with-user
                    "uu22"
                    {:RbacOwner/Upsert_E
                     {:Instance
                      {:RbacOwner/E
                       {:X 200}}}})))
             id1 (cn/id-attr e1)
             id2 (cn/id-attr e2)
             lookup (fn [id] {:RbacOwner/Lookup_E
                              {cn/id-attr id}})
             delete (fn [id] {:RbacOwner/Delete_E
                              {cn/id-attr id}})]
         (is (cn/instance-of? :RbacOwner/E e1))
         (is (cn/instance-of? :RbacOwner/E e2))
         (is (cn/same-instance?
              e2
              (first
               (tu/result
                (with-user "uu11" (lookup id2))))))
         (is (cn/same-instance?
              e1
              (first
               (tu/result
                (with-user "uu22" (lookup id1))))))
         (is (cn/same-instance?
              e1
              (first
               (tu/result
                (with-user "uu11" (delete id1))))))
         (is (= :error
                (:status
                 (first
                  (tu/eval-all-dataflows
                   (with-user "uu11" (delete id2)))))))
         (is (cn/same-instance?
              e2
              (first
               (tu/result
                (with-user "uu22" (delete id2)))))))))
   #(ei/reset-interceptors!)))

(deftest rbac
  (rbac-application)
  (rbac-with-owner))
