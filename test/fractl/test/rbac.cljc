(ns fractl.test.rbac
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [fractl.rbac.core :as rbac]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.evaluator.intercept :as ei]
            [fractl.auth]
            [fractl.lang.rbac :as lr]
            [fractl.lang.internal :as li]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship dataflow]]
            #?(:clj  [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest role-management
  (defcomponent :RoleMgmt
    (dataflow
     :RoleMgmt/CreateUsers
     {:Fractl.Kernel.Identity/User {:Email "abc@abc.com"}}
     {:Fractl.Kernel.Identity/User {:Email "xyz@xyz.com"}})
    (dataflow
     :RoleMgmt/CreateRoles
     {:Fractl.Kernel.Rbac/Role {:Name "r1"}}
     {:Fractl.Kernel.Rbac/Role {:Name "r2"}})
    (dataflow
     :RoleMgmt/AssignPrivileges
     {:Fractl.Kernel.Rbac/Privilege
      {:Name "p1"
       :Actions [:q# [:read :create :update]]
       :Resource [:q# [:A :B]]}}
     {:Fractl.Kernel.Rbac/Privilege
      {:Name "p2"
       :Actions [:q# [:read]]
       :Resource [:q# [:C]]}}
     {:Fractl.Kernel.Rbac/PrivilegeAssignment
      {:Role "r1" :Privilege "p1"}}
     {:Fractl.Kernel.Rbac/PrivilegeAssignment
      {:Role "r1" :Privilege "p2"}}
     {:Fractl.Kernel.Rbac/PrivilegeAssignment
      {:Role "r2" :Privilege "p2"}})
    (dataflow
     :RoleMgmt/AssignRoles
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "r1" :Assignee "abc@abc.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "r2" :Assignee "xyz@xyz.com"}}))
  (let [[r1 r2 r3 r4]
        (mapv tu/result [:RoleMgmt/CreateUsers :RoleMgmt/CreateRoles
                         :RoleMgmt/AssignPrivileges :RoleMgmt/AssignRoles])]
    (is (cn/instance-of? :Fractl.Kernel.Identity/User (first r1)))
    (is (cn/instance-of? :Fractl.Kernel.Rbac/Role (first r2)))
    (is (cn/instance-of? :Fractl.Kernel.Rbac/PrivilegeAssignment (first r3)))
    (is (cn/instance-of? :Fractl.Kernel.Rbac/RoleAssignment (first r4)))
    (let [ps1 (rbac/privileges "abc@abc.com")
          ps2 (rbac/privileges "xyz@xyz.com")
          p2 (first ps2)]
      (is (= (count ps1) 2))
      (is (= (count ps2) 1))
      (is (= [:read] (:Actions p2)))
      (is (= [:C] (:Resource p2))))))

(defn- with-user [email event]
  (cn/assoc-event-context-user
   email
   (cn/make-instance
    (if (keyword? event)
      {event {}}
      event))))

(def ^:privilege default-finalize ei/reset-interceptors!)

(defn- call-with-rbac
  ([f finalize]
   (is (rbac/init))
   (try
     (f)
     (finally
       (finalize))))
  ([f] (call-with-rbac f default-finalize)))

(defn- finalize-events []
  (lr/finalize-events tu/eval-all-dataflows))

(deftest basic-rbac-dsl
  (lr/reset-events!)
  (defcomponent :Brd
    (entity
     :Brd/E
     {:rbac [{:roles ["user"] :allow [:create]}
             {:roles ["manager"] :allow [:create :update :read]}]
      :Id {:type :Int :identity true}
      :X :Int})
    (dataflow
     :Brd/InitUsers
     {:Fractl.Kernel.Identity/User
      {:Email "u1@brd.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u2@brd.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u3@brd.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "user" :Assignee "u2@brd.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "manager" :Assignee "u1@brd.com"}}))
  (is (finalize-events))
  (is (cn/instance-of?
       :Fractl.Kernel.Rbac/RoleAssignment
       (tu/first-result {:Brd/InitUsers {}})))
  (let [e? (partial cn/instance-of? :Brd/E)]
    (call-with-rbac
     (fn []
       (is (= [:rbac] (ei/init-interceptors [:rbac])))
       (let [create-e (fn [id]
                        {:Brd/Create_E
                         {:Instance
                          {:Brd/E {:Id id :X (* id 100)}}}})
             update-e (fn [id]
                        {:Brd/Update_E
                         {:Id id :Data {:X (* id 200)}}})
             lookup-e (fn [id]
                        {:Brd/Lookup_E {:Id id}})
             test-lookup (fn [user factor err e]
                           (if err
                             (tu/is-error #(tu/eval-all-dataflows (with-user user (lookup-e e))))
                             (let [r (tu/first-result (with-user user (lookup-e e)))]
                               (is (e? r)) (is (= (:Id r) e)) (is (= (:X r) (* factor e))))))]
         (tu/is-error #(tu/eval-all-dataflows (create-e 1)))
         (is (e? (tu/first-result (with-user "u1@brd.com" (create-e 1)))))
         (is (e? (tu/first-result (with-user "u2@brd.com" (create-e 2)))))
         (tu/is-error #(tu/eval-all-dataflows (with-user "u3@brd.com" (create-e 3))))
         (let [t1 (partial test-lookup "u1@brd.com" 100 false)
               t2 (partial test-lookup "u2@brd.com" 100)
               t3 (partial test-lookup "u3@brd.com" 100 true)]
           (t1 1) (t1 2) (t2 false 2) (t2 true 1)
           (test-lookup "u3@brd.com" 100 true 1)
           (t3 1) (t3 2))
         (let [test-update (fn [user err e]
                             (if err
                               (tu/is-error #(tu/eval-all-dataflows (with-user user (update-e e))))
                               (let [r (tu/first-result (with-user user (update-e e)))]
                                 (is (e? r)) (is (= (:Id r) e)) (is (= (:X r) (* e 200))))))
               t1 (partial test-update "u1@brd.com" false)
               t2 (partial test-update "u2@brd.com")
               t3 (partial test-update "u3@brd.com" true)]
           (t1 1) (t1 2)
           (test-lookup "u1@brd.com" 200 false 1)
           (test-lookup "u1@brd.com" 200 false 2)
           (t2 false 2)
           (test-lookup "u2@brd.com" 200 false 2)
           (t2 true 1)
           (t3 1) (t3 2)))))))
