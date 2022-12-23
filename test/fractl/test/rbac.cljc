(ns fractl.test.rbac
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [fractl.auth.model]
            [fractl.rbac.core :as rbac]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.evaluator.intercept :as ei]
            [fractl.auth]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship dataflow]]
            #?(:clj  [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest role-management
  (defcomponent :RoleMgmt
    (dataflow
     :RoleMgmt/CreateUsers
     {:Kernel.Identity/User {:Email "abc@abc.com"}}
     {:Kernel.Identity/User {:Email "xyz@xyz.com"}})
    (dataflow
     :RoleMgmt/CreateRoles
     {:Kernel.RBAC/Role {:Name "r1"}}
     {:Kernel.RBAC/Role {:Name "r2"}})
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
      {:Role "r1" :Assignee "abc@abc.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r2" :Assignee "xyz@xyz.com"}}))
  (let [[r1 r2 r3 r4]
        (mapv tu/result [:RoleMgmt/CreateUsers :RoleMgmt/CreateRoles
                         :RoleMgmt/AssignPrivileges :RoleMgmt/AssignRoles])]
    (is (cn/instance-of? :Kernel.Identity/User (first r1)))
    (is (cn/instance-of? :Kernel.RBAC/Role (first r2)))
    (is (cn/instance-of? :Kernel.RBAC/PrivilegeAssignment (first r3)))
    (is (cn/instance-of? :Kernel.RBAC/RoleAssignment (first r4)))
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

(defn- rbac-application []
  (defcomponent :PrivTest
    (entity
     :PrivTest/User
     {:User {:ref :Kernel.Identity/User.Email}})
    (entity
     :PrivTest/E
     {:X :Kernel/Int
      :Y {:type :Kernel/Int :optional true}})
    (dataflow
     :PrivTest/CreateSuperUser
     {:PrivTest/User
      {:User rbac/default-superuser-email}})
    (dataflow
     :PrivTest/CreateUsers
     {:Kernel.Identity/User
      {:Email "u11@u11.com"}}
     {:Kernel.Identity/User
      {:Email "u33@u33.com"}}
     {:Kernel.Identity/User
      {:Email "u22@u22.com"}}
     {:PrivTest/User
      {:User "u11@u11.com"}}
     {:PrivTest/User
      {:User "u33@u33.com"}}
     {:PrivTest/User
      {:User "u22@u22.com"}})
    (dataflow
     :PrivTest/CreatePrivileges
     {:Kernel.RBAC/Role {:Name "r11"}}
     {:Kernel.RBAC/Role {:Name "r33"}}
     {:Kernel.RBAC/Role {:Name "r22"}}
     {:Kernel.RBAC/Privilege
      {:Name "p11"
       :Actions [:q# [:read :upsert]]
       :Resource [:q# [:PrivTest/E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p22"
       :Actions [:q# [:eval]]
       :Resource [:q# [:PrivTest/Upsert_E
                       :PrivTest/UpdateE
                       :PrivTest/UpdateEX]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p33"
       :Actions [:q# [:read]]
       :Resource [:q# [:PrivTest/E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p44"
       :Actions [:q# [:eval]]
       :Resource [:q# [:PrivTest/Lookup_E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "p55"
       :Actions [:q# [:read :upsert]]
       :Resource [:q# [:PrivTest/E.X (tu/append-id :PrivTest/E)]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r11" :Privilege "p11"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r11" :Privilege "p22"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r22" :Privilege "p33"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r22" :Privilege "p44"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r22" :Privilege "p44"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r33" :Privilege "p22"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r33" :Privilege "p44"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "r33" :Privilege "p55"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r11" :Assignee "u11@u11.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r33" :Assignee "u33@u33.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "r22" :Assignee "u22@u22.com"}})
    (dataflow
     :PrivTest/UpdateE
     {:PrivTest/E
      {tu/q-id-attr :PrivTest/UpdateE.E
       :X :PrivTest/UpdateE.X
       :Y :PrivTest/UpdateE.Y}})
    (dataflow
     :PrivTest/UpdateEX
     {:PrivTest/E
      {tu/q-id-attr :PrivTest/UpdateEX.E
       :X :PrivTest/UpdateEX.X}}))
  (call-with-rbac
   (fn []
     (let [su (first (tu/result :PrivTest/CreateSuperUser))]
       (is (cn/instance-of? :PrivTest/User su))
       (is (= rbac/default-superuser-email (:User su)))
       (is (= [:rbac] (ei/init-interceptors [:rbac])))
       (let [u2 (first
                 (tu/result
                  (with-user rbac/default-superuser-email :PrivTest/CreateUsers)))]
         (is (cn/instance-of? :PrivTest/User u2))
         (is (= "u22@u22.com" (:User u2))))
       (let [r2 (first
                 (tu/result
                  (with-user rbac/default-superuser-email :PrivTest/CreatePrivileges)))]
         (is (cn/instance-of? :Kernel.RBAC/RoleAssignment r2))
         (is (and (= "r22" (:Role r2)) (= "u22@u22.com" (:Assignee r2)))))
       (tu/is-error
        #(ev/eval-all-dataflows
          (cn/make-instance
           {:PrivTest/Upsert_E
            {:Instance
             {:PrivTest/E
              {:X 100 :Y 10}}}})))
       (tu/is-error
        #(ev/eval-all-dataflows
          (with-user
            "u22@u22.com"
            {:PrivTest/Upsert_E
             {:Instance
              {:PrivTest/E
               {:X 200 :Y 20}}}})))
       (let [inst (first
                   (tu/result
                    (with-user
                      "u11@u11.com"
                      {:PrivTest/Upsert_E
                       {:Instance
                        {:PrivTest/E
                         {:X 100 :Y 10}}}})))
             id (cn/id-attr inst)
             lookup {:PrivTest/Lookup_E
                     {cn/id-attr id}}]
         (is (cn/instance-of? :PrivTest/E inst))
         (tu/is-error
          #(ev/eval-all-dataflows
            (with-user "u11@u11.com" lookup)))
         (let [partial-inst?
               (fn [x inst]
                 (is (cn/instance-of? :PrivTest/E inst))
                 (is (= id (cn/id-attr inst)))
                 (is (not (:Y inst)))
                 (is (= x (:X inst))))]
           (partial-inst?
            100
            (tu/first-result
             (with-user "u33@u33.com" lookup)))
           (tu/is-error
            #(ev/eval-all-dataflows
              (with-user
                "u33@u33.com"
                {:PrivTest/UpdateE
                 {:E id :X 1000 :Y 2000}})))
           (partial-inst?
            1000
            (get-in
             (tu/first-result
              (with-user
                "u33@u33.com"
                {:PrivTest/UpdateEX
                 {:E id :X 1000 :Y 2000}}))
             [:transition :to])))
         (let [inst2 (first
                      (tu/result
                       (with-user "u22@u22.com" lookup)))]
           (is (cn/instance-of? :PrivTest/E inst2))
           (is (= id (cn/id-attr inst2)))
           (is (= 1000 (:X inst2)))
           (is (= 10 (:Y inst2)))))))))

(defn- rbac-with-owner []
  (is (= [:instance-meta] (ei/init-interceptors [:instance-meta])))
  (defcomponent :RbacOwner
    (entity
     :RbacOwner/User
     {:User {:ref :Kernel.Identity/User.Email}})
    (entity
     :RbacOwner/E
     {:X :Kernel/Int})
    (dataflow
     :RbacOwner/CreateSuperUser
     {:RbacOwner/User
      {:User rbac/default-superuser-email}})
    (dataflow
     :RbacOwner/CreateUsers
     {:Kernel.Identity/User
      {:Email "uu11@uu11.com"}}
     {:Kernel.Identity/User
      {:Email "uu22@uu22.com"}}
     {:RbacOwner/User
      {:User "uu11@uu11.com"}}
     {:RbacOwner/User
      {:User "uu22@uu22.com"}})
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
      {:Role "rr11" :Assignee "uu11@uu11.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "rr11" :Assignee "uu22@uu22.com"}}))
  (call-with-rbac
   (fn []
     (let [su (first (tu/result :RbacOwner/CreateSuperUser))]
       (is (cn/instance-of? :RbacOwner/User su))
       (is (= rbac/default-superuser-email (:User su)))
       (is (= [:rbac] (ei/init-interceptors [:rbac])))
       (let [u2 (first
                 (tu/result
                  (with-user rbac/default-superuser-email :RbacOwner/CreateUsers)))]
         (is (cn/instance-of? :RbacOwner/User u2))
         (is (= "uu22@uu22.com" (:User u2))))
       (let [r1 (first
                 (tu/result
                  (with-user rbac/default-superuser-email :RbacOwner/CreatePrivileges)))]
         (is (cn/instance-of? :Kernel.RBAC/RoleAssignment r1))
         (is (and (= "rr11" (:Role r1)) (= "uu22@uu22.com" (:Assignee r1)))))
       (let [e1 (first
                 (tu/result
                  (with-user
                    "uu11@uu11.com"
                    {:RbacOwner/Upsert_E
                     {:Instance
                      {:RbacOwner/E
                       {:X 100}}}})))
             e2 (first
                 (tu/result
                  (with-user
                    "uu22@uu22.com"
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
                (with-user "uu11@uu11.com" (lookup id2))))))
         (is (cn/same-instance?
              e1
              (first
               (tu/result
                (with-user "uu22@uu22.com" (lookup id1))))))
         (is (cn/same-instance?
              e1
              (first
               (tu/result
                (with-user "uu11@uu11.com" (delete id1))))))
         (is (= :error
                (:status
                 (first
                  (tu/eval-all-dataflows
                   (with-user "uu11@uu11.com" (delete id2)))))))
         (is (cn/same-instance?
              e2
              (first
               (tu/result
                (with-user "uu22@uu22.com" (delete id2)))))))))))

(deftest basic
  (rbac-application)
  (rbac-with-owner))

(deftest hierarchy
  (defcomponent :RbacH
    (entity
     :RbacH/E
     {:X :Kernel/Int})
    (dataflow
     :RbacH/CreateUsers
     {:Kernel.Identity/User
      {:Email "uh11@uh11.com"}}
     {:Kernel.Identity/User
      {:Email "uh22@uh22.com"}})
    (dataflow
     :RbacH/CreatePrivileges
     {:Kernel.RBAC/Role {:Name "rh11"}}
     {:Kernel.RBAC/Role {:Name "rh22"}}
     {:Kernel.RBAC/Privilege
      {:Name "ph11"
       :Actions [:q# [:read :upsert]]
       :Resource [:q# [:RbacH/E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "ph22"
       :Actions [:q# [:eval]]
       :Resource [:q# [:RbacH/Upsert_E :RbacH/Lookup_E]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "rh11" :Privilege "ph11"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "rh11" :Privilege "ph22"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "rh11" :Assignee "uh11@uh11.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "rh22" :Assignee "uh22@uh22.com"}})
    (dataflow
     :RbacH/AssignParent
     {:Kernel.RBAC/AssignRelationship
      {:Parent "rh22" :Child "rh11"}}))
  (call-with-rbac
   (fn []
     (is (= [:rbac] (ei/init-interceptors [:rbac])))
     (let [u2 (first
               (tu/result
                (with-user rbac/default-superuser-email :RbacH/CreateUsers)))]
       (is (cn/instance-of? :Kernel.Identity/User u2))
       (is (= "uh22@uh22.com" (:Email u2))))
     (let [r2 (first
               (tu/result
                (with-user rbac/default-superuser-email :RbacH/CreatePrivileges)))]
       (is (cn/instance-of? :Kernel.RBAC/RoleAssignment r2))
       (is (and (= "rh22" (:Role r2)) (= "uh22@uh22.com" (:Assignee r2)))))
     (let [upsert-event {:RbacH/Upsert_E
                         {:Instance
                          {:RbacH/E
                           {:X 100}}}}
           mk-lookup-event (fn [id]
                             {:RbacH/Lookup_E
                              {cn/id-attr id}})]
       (tu/is-error
        #(tu/eval-all-dataflows
          (with-user "uh22@uh22.com" upsert-event)))
       (let [ok-test
             (fn [user]
               (let [u (tu/first-result (with-user user upsert-event))
                     id (cn/id-attr u)
                     u2 (tu/first-result (with-user user (mk-lookup-event id)))]
                 (is (cn/instance-of? :RbacH/E u))
                 (is (cn/same-instance? u u2))
                 u))]
         (tu/is-error
          #(tu/eval-all-dataflows
            (with-user "uh22@uh22.com" (mk-lookup-event (cn/id-attr (ok-test "uh11@uh11.com"))))))
         (let [r (tu/first-result
                  (with-user rbac/default-superuser-email :RbacH/AssignParent))]
           (is (cn/instance-of? :Kernel.RBAC/RoleRelationship r))
           (is (= "rh22" (:Parent r)))
           (is (= "rh11" (:Child r))))
         (rbac/force-reload-privileges!)
         (let [u (ok-test "uh22@uh22.com")]
           (cn/instance-of? :RbacH/E u)))))))

(deftest instance-level-rbac
  (defcomponent :Ilr
    (entity
     :Ilr/E
     {:Id {:type :Kernel/String :identity true}
      :X :Kernel/Int})
    (dataflow
     :Ilr/CreateUsers
     {:Kernel.Identity/User
      {:Email "ilr_u1@ilr.com"}}
     {:Kernel.Identity/User
      {:Email "ilr_u2@ilr.com"}}
     {:Kernel.Identity/User
      {:Email "ilr_u3@ilr.com"}})
    (dataflow
     :Ilr/AssignRoles
     {:Kernel.RBAC/Role {:Name "ilr_r1"}}
     {:Kernel.RBAC/Privilege
      {:Name "ilr_p1"
       :Actions [:q# [:read :upsert :delete]]
       :Resource [:q# [:Ilr/E]]}}
     {:Kernel.RBAC/Privilege
      {:Name "ilr_p2"
       :Actions [:q# [:eval]]
       :Resource [:q# [:Ilr/CreateE :Ilr/UpdateE
                       :Ilr/DeleteE :Ilr/LookupE
                       :Ilr/UpdateInstancePrivs]]}}
     {:Kernel.RBAC/Privilege
      {:Name "ilr_p3"
       :Actions [:q# [:read :upsert :delete]]
       :Resource [:q# [:Kernel.RBAC/InstancePrivilegeAssignment]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "ilr_r1" :Privilege "ilr_p1"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "ilr_r1" :Privilege "ilr_p2"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "ilr_r1" :Privilege "ilr_p3"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "ilr_r1" :Assignee "ilr_u1@ilr.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "ilr_r1" :Assignee "ilr_u2@ilr.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "ilr_r1" :Assignee "ilr_u3@ilr.com"}})
    (event
     :Ilr/CreateE
     {:X :Kernel/Int
      :Id :Kernel/String
      :Assignee :Kernel/String})
    (dataflow
     :Ilr/CreateE
     {:Ilr/E {:Id :Ilr/CreateE.Id :X :Ilr/CreateE.X} :as :E}
     {:Kernel.RBAC/InstancePrivilegeAssignment
      {:Actions [:q# [:read :upsert]]
       :Filter [:q# [:read]]
       :Resource [:q# :Ilr/E]
       :ResourceId :E.Id
       :Assignee :Ilr/CreateE.Assignee}}
     :E)
    (dataflow
     :Ilr/UpdateE
     {:Ilr/E {:Id? :Ilr/UpdateE.Id :X :Ilr/UpdateE.X}})
    (dataflow
     :Ilr/LookupE
     {:Ilr/E {:Id? :Ilr/LookupE.Id}})
    (dataflow
     :Ilr/DeleteE
     [:delete :Ilr/E {:Id :Ilr/DeleteE.Id}])
    (dataflow
     :Ilr/UpdateInstancePrivs
     {:Kernel.RBAC/InstancePrivilegeAssignment
      {:Actions [:q# [:read :upsert :delete]]
       :Filter [:q# [:read]]
       :Resource [:q# :Ilr/E]
       :ResourceId :Ilr/UpdateInstancePrivs.Id
       :Assignee :Ilr/UpdateInstancePrivs.Assignee}}))
  (defn- rbac-setup [event-name result-type]
    (is (cn/instance-of?
         result-type
         (first
          (tu/result
           (with-user rbac/default-superuser-email event-name))))))
  (call-with-rbac
   (fn []
     (is (= [:rbac :instance-meta] (ei/init-interceptors [:rbac :instance-meta])))
     (rbac-setup :Ilr/CreateUsers :Kernel.Identity/User)
     (rbac-setup :Ilr/AssignRoles :Kernel.RBAC/RoleAssignment)
     (let [es (mapv
               #(tu/result
                 (with-user
                   "ilr_u1@ilr.com"
                   {:Ilr/CreateE
                    {:X (second %) :Id (first %)
                     :Assignee "ilr_u2@ilr.com"}}))
               [["123" 100] ["564" 200] ["222" 300]])
           e (first es)]
       (is (cn/instance-of? :Ilr/E e))
       (defn- update-e [fail? id user new-x]
         (let [e1 (:to
                   (:transition
                    (tu/first-result
                     (with-user
                       user
                       {:Ilr/UpdateE {:Id id :X new-x}}))))]
           (if fail?
             (is (not e1))
             (is (and (cn/instance-of? :Ilr/E e1)
                      (= (:Id e1) (:Id e))
                      (= (:X e1) new-x))))))
       (update-e false "123" "ilr_u1@ilr.com" 200)
       (update-e false "123" "ilr_u2@ilr.com" 300)
       (update-e true "123" "ilr_u3@ilr.com" 400)
       (defn- lookup-e [id user]
         (let [e1 (tu/first-result
                   (with-user user
                     {:Ilr/LookupE {:Id id}}))]
           (is (and (cn/instance-of? :Ilr/E e1)
                    (= (:Id e1) (:Id e))
                    (= (:X e1) 300)))))
       (doseq [user ["ilr_u1@ilr.com"
                     "ilr_u2@ilr.com"
                     "ilr_u3@ilr.com"]]
         (lookup-e "123" user))
       (defn- delete-e [fail? id user]
         (let [e1 (tu/first-result
                   (with-user user
                     {:Ilr/DeleteE {:Id id}}))]
           (if fail?
             (is (not e1))
             (do (is (cn/instance-of? :Ilr/E e1))
                 (is (= (:Id e1) id))))))
       (delete-e false "123" "ilr_u1@ilr.com")
       (delete-e false "222" "ilr_u1@ilr.com")
       (delete-e true "564" "ilr_u2@ilr.com")
       (defn- change-inst-priv [id user assignee]
         (tu/first-result
          (with-user
            user
            {:Ilr/UpdateInstancePrivs
             {:Id id :Assignee assignee}})))
       ;; Only owner or superuser can set instance privilege.
       (is (not (change-inst-priv "564" "ilr_u2@ilr.com" "ilr_u2@ilr.com")))
       (let [a (change-inst-priv "564" "ilr_u1@ilr.com" "ilr_u2@ilr.com")]
         (is (cn/instance-of?
              :Kernel.RBAC/InstancePrivilegeAssignment
              (:to (:transition a))))
         (delete-e false "564" "ilr_u2@ilr.com"))))))

(deftest issue-711-inherit-entity-priv
  (defcomponent :I711A
    (entity
     :I711A/E1
     {:X {:type :Kernel/Int :identity true}})
    (entity
     :I711A/E2
     {:Y {:type :Kernel/Int :identity true}})
    (relationship
     :I711A/R1
     {:meta {:contains [:I711A/E1 :I711A/E2]
             :rbac {:inherit {:entity true}}}})
    (dataflow
     :I711A/CreateUsers
     {:Kernel.Identity/User
      {:Email "u1@i711a.com"}})
    (dataflow
     :I711A/AssignRoles
     {:Kernel.RBAC/Role {:Name "i711a_r1"}}
     {:Kernel.RBAC/Privilege
      {:Name "i711a_p1"
       :Actions [:q# [:read :upsert :delete]]
       :Resource [:q# [:I711A/E1 :I711A/R1]]}}
     {:Kernel.RBAC/Privilege
      {:Name "i711a_p2"
       :Actions [:q# [:eval]]
       :Resource [:q# [:I711A/Upsert_E1 :I711A/CreateE2]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711a_r1" :Privilege "i711a_p1"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711a_r1" :Privilege "i711a_p2"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "i711a_r1" :Assignee "u1@i711a.com"}})
    (event
     :I711A/CreateE2
     {:X :Kernel/Int :Y :Kernel/Int})
    (dataflow
     :I711A/CreateE2
     {:I711A/E1 {:X? :I711A/CreateE2.X} :as :E1}
     {:I711A/E2
      {:Y :I711A/CreateE2.Y}
      :-> [{:I711A/R1 {}} :E1]}))
  (defn- rbac-setup [event-name result-type]
    (is (cn/instance-of?
         result-type
         (first
          (tu/result
           (with-user rbac/default-superuser-email event-name))))))
  (defn- create-e1 [x expect-error]
    (let [f #(tu/result
              (with-user
                "u1@i711a.com"
                {:I711A/Upsert_E1
                 {:Instance {:I711A/E1 {:X x}}}}))]
      (if expect-error
        (tu/is-error f)
        (f))))
  (call-with-rbac
   (fn []
     (is (= [:rbac :instance-meta] (ei/init-interceptors [:rbac :instance-meta])))
     (rbac-setup :I711A/CreateUsers :Kernel.Identity/User)
     (is (create-e1 10 true))
     (rbac-setup :I711A/AssignRoles :Kernel.RBAC/RoleAssignment)
     (rbac/force-reload-privileges!)
     (is (cn/instance-of? :I711A/E1 (first (create-e1 10 false))))
     (let [r (tu/result (with-user "u1@i711a.com" {:I711A/CreateE2 {:X 10 :Y 100}}))]
       (is (cn/instance-of? :I711A/E2 r))
       (is (cn/instance-of? :I711A/R1 (first (:-> r))))))))

(deftest issue-711-inherit-instance-priv
  (defcomponent :I711B
    (entity
     :I711B/E1
     {:X {:type :Kernel/Int :identity true}})
    (entity
     :I711B/E2
     {:Y {:type :Kernel/Int :identity true}
      :K :Kernel/Int})
    (relationship
     :I711B/R1
     {:meta {:contains [:I711B/E1 :I711B/E2]
             :rbac {:inherit {:instance true}}}})
    (dataflow
     :I711B/CreateUsers
     {:Kernel.Identity/User
      {:Email "u1@i711b.com"}}
     {:Kernel.Identity/User
      {:Email "u2@i711b.com"}})
    (dataflow
     :I711B/AssignRoles
     {:Kernel.RBAC/Role {:Name "i711b_r1"}}
     {:Kernel.RBAC/Role {:Name "i711b_r2"}}
     {:Kernel.RBAC/Privilege
      {:Name "i711b_p1"
       :Actions [:q# [:read :upsert :delete]]
       :Resource [:q# [:I711B/E1 :I711B/R1 :I711B/E2]]}}
     {:Kernel.RBAC/Privilege
      {:Name "i711b_p2"
       :Actions [:q# [:eval]]
       :Resource [:q# [:I711B/Upsert_E1 :I711B/CreateE2
                       :I711B/UpdateE2 :I711B/Lookup_E1]]}}
     {:Kernel.RBAC/Privilege
      {:Name "i711b_p3"
       :Actions [:q# [:eval :upsert :read]]
       :Resource [:q# [:I711B/AssignInstancePriv
                       :Kernel.RBAC/InstancePrivilegeAssignment]]}}
     {:Kernel.RBAC/Privilege
      {:Name "i711b_p4"
       :Actions [:q# [:read]]
       :Resource [:q# [:I711B/E1 :I711B/R1 :I711B/E2]]}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711b_r1" :Privilege "i711b_p1"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711b_r1" :Privilege "i711b_p2"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711b_r1" :Privilege "i711b_p3"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711b_r2" :Privilege "i711b_p2"}}
     {:Kernel.RBAC/PrivilegeAssignment
      {:Role "i711b_r2" :Privilege "i711b_p4"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "i711b_r1" :Assignee "u1@i711b.com"}}
     {:Kernel.RBAC/RoleAssignment
      {:Role "i711b_r2" :Assignee "u2@i711b.com"}})
    (event
     :I711B/AssignInstancePriv
     {:X :Kernel/Int
      :User :Kernel/String})
    (dataflow
     :I711B/AssignInstancePriv
     {:Kernel.RBAC/InstancePrivilegeAssignment
      {:Actions [:q# [:read :upsert]]
       :Resource [:q# :I711B/E1]
       :ResourceId :I711B/AssignInstancePriv.X
       :Assignee :I711B/AssignInstancePriv.User}})
    (event
     :I711B/CreateE2
     {:X :Kernel/Int :Y :Kernel/Int :K :Kernel/Int})
    (dataflow
     :I711B/CreateE2
     {:I711B/E1 {:X? :I711B/CreateE2.X} :as :E1}
     {:I711B/E2
      {:Y :I711B/CreateE2.Y
       :K :I711B/CreateE2.K}
      :-> [{:I711B/R1 {}} :E1]})
    (event
     :I711B/UpdateE2
     {:X :Kernel/Int :Y :Kernel/Int :K :Kernel/Int})
    (dataflow
     :I711B/UpdateE2
     {:I711B/E2
      {:Y? :I711B/UpdateE2.Y
       :K :I711B/UpdateE2.K}
      :-> [:I711B/R1?
           {:I711B/E1 {:X? :I711B/UpdateE2.X}}]}))
  (defn- rbac-setup [event-name result-type]
    (is (cn/instance-of?
         result-type
         (first
          (tu/result
           (with-user rbac/default-superuser-email event-name))))))
  (defn- create-e1 [x expect-error]
    (let [f #(tu/result
              (with-user
                "u1@i711b.com"
                {:I711B/Upsert_E1
                 {:Instance {:I711B/E1 {:X x}}}}))]
      (if expect-error
        (tu/is-error f)
        (f))))
  (call-with-rbac
   (fn []
     (is (= [:rbac :instance-meta] (ei/init-interceptors [:rbac :instance-meta])))
     (rbac-setup :I711B/CreateUsers :Kernel.Identity/User)
     (is (create-e1 10 true))
     (rbac-setup :I711B/AssignRoles :Kernel.RBAC/RoleAssignment)
     (rbac/force-reload-privileges!)
     (is (cn/instance-of? :I711B/E1 (first (create-e1 10 false))))
     (let [r (tu/result (with-user "u1@i711b.com" {:I711B/CreateE2 {:X 10 :Y 100 :K 3}}))]
       (is (cn/instance-of? :I711B/E2 r))
       (is (cn/instance-of? :I711B/R1 (first (:-> r)))))
     (is (not (tu/result (with-user "u2@i711b.com" {:I711B/CreateE2 {:X 10 :Y 200 :K 4}}))))
     (is (not (tu/result (with-user "u2@i711b.com" {:I711B/UpdateE2 {:X 10 :Y 100 :K 4}}))))
     (let [r (tu/first-result (with-user "u1@i711b.com" {:I711B/UpdateE2 {:X 10 :Y 100 :K 4}}))]
       (let [from (:from (:transition r))
             to (:to (:transition r))]
         (is (= 3 (:K from)))
         (is (= 4 (:K to)))))
     (is (cn/instance-of?
          :Kernel.RBAC/InstancePrivilegeAssignment
          (tu/first-result
           (with-user "u1@i711b.com"
             {:I711B/AssignInstancePriv
              {:X 10 :User "u2@i711b.com"}}))))
     (let [r (tu/first-result
              (with-user "u2@i711b.com"
                {:I711B/UpdateE2 {:X 10 :Y 100 :K 5}}))
           from (:from (:transition r))
           to (:to (:transition r))]
       (is (= 4 (:K from)))
       (is (= 5 (:K to)))))))
