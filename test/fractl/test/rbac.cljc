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
     {:rbac [{:roles ["brd-user"] :allow [:create]}
             {:roles ["brd-manager"] :allow [:create :update :read]}]
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
      {:Role "brd-user" :Assignee "u2@brd.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "brd-manager" :Assignee "u1@brd.com"}}))
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
             delete-e (fn [id] {:Brd/Delete_E {:Id id}})
             test-lookup (fn [user factor err e]
                           (if err
                             (let [r (tu/eval-all-dataflows (with-user user (lookup-e e)))]
                               (or (tu/not-found? r) (tu/is-error #(identity r))))
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
           (t3 1) (t3 2))
         (tu/is-error #(tu/eval-all-dataflows (with-user "u2@brd.com" (delete-e 1))))
         (tu/is-error #(tu/eval-all-dataflows (with-user "u1@brd.com" (delete-e 2))))
         (test-lookup "u1@brd.com" 200 false 1)
         (test-lookup "u1@brd.com" 200 false 2)
         (let [r (tu/first-result (with-user "u2@brd.com" (delete-e 2)))]
           (is (e? r)) (is (= (:Id r) 2)))
         (test-lookup "u1@brd.com" 200 true 2)
         (test-lookup "u2@brd.com" 200 true 2)
         (let [r (tu/first-result (with-user "u1@brd.com" (delete-e 1)))]
           (is (e? r)) (is (= (:Id r) 1)))
         (test-lookup "u1@brd.com" 200 true 1))))))

(deftest rbac-with-contains-relationship
  (lr/reset-events!)
  (defcomponent :Wcr
    (entity
     :Wcr/E
     {:rbac [{:roles ["wcr-user"] :allow [:create :update :read]}]
      :Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Wcr/F
     {:Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :Wcr/R
     {:meta {:contains [:Wcr/E :Wcr/F]}})
    (dataflow
     :Wcr/InitUsers
     {:Fractl.Kernel.Identity/User
      {:Email "u1@wcr.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u2@wcr.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "wcr-user" :Assignee "u1@wcr.com"}}))
  (is (finalize-events))
  (is (cn/instance-of?
       :Fractl.Kernel.Rbac/RoleAssignment
       (tu/first-result {:Wcr/InitUsers {}})))
  (let [e? (partial cn/instance-of? :Wcr/E)]
    (call-with-rbac
     (fn []
       (is (= [:rbac] (ei/init-interceptors [:rbac])))
       (let [fq (partial li/as-fully-qualified-path :Wcr)
             e? (partial cn/instance-of? :Wcr/E)
             f? (partial cn/instance-of? :Wcr/F)
             create-e (fn [id]
                        {:Wcr/Create_E
                         {:Instance
                          {:Wcr/E {:Id id :X (* id 100)}}}})
             delete-e (fn [id]
                        {:Wcr/Delete_E {:Id id}})
             create-f (fn [e id]
                        {:Wcr/Create_F
                         {:Instance
                          {:Wcr/F
                           {:Id id
                            :Y (* 5 id)}}
                          li/path-attr (str "/E/" e "/R")}})
             lookup-fs (fn [e]
                         {:Wcr/LookupAll_F
                          {li/path-attr (fq (str "path://E/" e "/R/F/%"))}})
             with-u1 (partial with-user "u1@wcr.com")
             e1 (tu/first-result (with-u1 (create-e 1)))
             [f1 f2 :as fs] (mapv #(tu/first-result (with-u1 (create-f 1 %))) [10 20])]
         (is (e? e1))
         (is (= 2 (count fs)))
         (is (every? f? fs))
         (is (every? (fn [f] (some #{"u1@wcr.com"} (cn/owners f))) fs))
         (is (tu/is-error #(tu/eval-all-dataflows (with-user "u2@wcr.com" (create-e 2)))))
         (let [fs (tu/result (with-u1 (lookup-fs 1)))]
           (is (= 2 (count fs)))
           (is (every? f? fs)))
         (is (e? (tu/first-result (with-u1 (delete-e 1)))))
         (is (tu/not-found? (tu/eval-all-dataflows (with-u1 (lookup-fs 1))))))))))

(deftest instance-privs
  (lr/reset-events!)
  (defcomponent :Ipv
    (entity
     :Ipv/E
     {:rbac [{:roles ["ipv-user"] :allow [:create :update :read]}
             {:roles ["ipv-guest"] :allow [:read]}]
      :Id {:type :Int :identity true}
      :X :Int})
    (dataflow
     :Ipv/InitUsers
     {:Fractl.Kernel.Identity/User
      {:Email "u1@ipv.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u2@ipv.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "ipv-user" :Assignee "u1@ipv.com"}})
    {:Fractl.Kernel.Rbac/RoleAssignment
     {:Role "ipv-guest" :Assignee "u2@ipv.com"}})
  (is (finalize-events))
  (is (cn/instance-of?
       :Fractl.Kernel.Rbac/RoleAssignment
       (tu/first-result {:Ipv/InitUsers {}})))
  (call-with-rbac
   (fn []
     (is (= [:rbac] (ei/init-interceptors [:rbac])))
     (let [e? (partial cn/instance-of? :Ipv/E)
           create-e (fn [user id]
                      (tu/first-result
                       (with-user
                         user
                         {:Ipv/Create_E
                          {:Instance
                           {:Ipv/E {:Id id :X (* id 100)}}}})))
           update-e (fn [user id x]
                      (tu/first-result
                       (with-user
                         user
                         {:Ipv/Update_E
                          {:Id id
                           :Data {:X x}}})))
           lookup-e (fn [user id]
                      (tu/first-result
                       (with-user
                         user
                         {:Ipv/Lookup_E
                          {:Id id}})))
           inst-priv (fn [owner assignee actions id]
                       (tu/first-result
                        (with-user
                          owner
                          {:Fractl.Kernel.Rbac/Create_InstancePrivilegeAssignment
                           {:Instance
                            {:Fractl.Kernel.Rbac/InstancePrivilegeAssignment
                             {:Resource :Ipv/E
                              :ResourceId id
                              :Assignee assignee
                              :Actions actions}}}})))
           del-inst-priv (fn [owner assignee id] (inst-priv owner assignee nil id))
           ip? (partial cn/instance-of? :Fractl.Kernel.Rbac/InstancePrivilegeAssignment)
           e1 (create-e "u1@ipv.com" 1)]
       (is (e? e1))
       (is (not (create-e "u2@ipv.com" 2)))
       (is (cn/same-instance? e1 (lookup-e "u1@ipv.com" 1)))
       (is (not (lookup-e "u2@ipv.com" 1)))
       (is (e? (update-e "u1@ipv.com" 1 3000)))
       (is (not (update-e "u2@ipv.com" 1 5000)))
       (is (ip? (inst-priv "u1@ipv.com" "u2@ipv.com" [:read :update] 1)))
       (let [e (lookup-e "u1@ipv.com" 1)]
         (is (= 3000 (:X e)))
         (is (= [:read :update] (cn/instance-privileges-for-user e "u2@ipv.com")))
         (is (cn/same-instance? e (lookup-e "u2@ipv.com" 1)))
         (is (e? (update-e "u2@ipv.com" 1 5000)))
         (is (ip? (del-inst-priv "u1@ipv.com" "u2@ipv.com" 1)))
         (let [e (lookup-e "u1@ipv.com" 1)]
           (is (= 5000 (:X e)))
           (is (not (cn/instance-privileges-for-user e "u2@ipv.com")))
           (is (not (update-e "u2@ipv.com" 1 8000)))
           (is (not (lookup-e "u2@ipv.com" 1)))
           (is (cn/same-instance? e (lookup-e "u1@ipv.com" 1)))))))))

(deftest issue-1018
  (lr/reset-events!)
  (defcomponent :I1018
    (entity
     :I1018/A
     {:rbac [{:roles ["i1018-admin"] :allow [:create :update :read]}]
      :Id {:type :Int :identity true}
      :X :Int})
    (entity
     :I1018/B
     {:rbac [{:roles ["i1018-user"] :allow [:create :update :read]}]
      :Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :I1018/R
     {:meta {:contains [:I1018/A :I1018/B]}})
    (dataflow
     :I1018/InitUsers
     {:Fractl.Kernel.Identity/User
      {:Email "u1@i1018.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u2@i1018.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "i1018-admin" :Assignee "u1@i1018.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "i1018-user" :Assignee "u2@i1018.com"}}))
  (is (finalize-events))
  (is (cn/instance-of?
       :Fractl.Kernel.Rbac/RoleAssignment
       (tu/first-result {:I1018/InitUsers {}})))
  (call-with-rbac
   (fn []
     (is (= [:rbac] (ei/init-interceptors [:rbac])))
     (let [fq (partial li/as-fully-qualified-path :I1018)
           a? (partial cn/instance-of? :I1018/A)
           b? (partial cn/instance-of? :I1018/B)
           create-a (fn [id]
                      {:I1018/Create_A
                       {:Instance
                        {:I1018/A {:Id id :X (* id 100)}}}})
           create-b (fn [a id]
                      {:I1018/Create_B
                       {:Instance
                        {:I1018/B
                         {:Id id
                          :Y (* 5 id)}}
                        li/path-attr (str "/A/" a "/R")}})
           lookup-bs (fn [a]
                       {:I1018/LookupAll_B
                        {li/path-attr (fq (str "path://A/" a "/R/B/%"))}})
           with-u1 (partial with-user "u1@i1018.com")
           with-u2 (partial with-user "u2@i1018.com")
           a1 (tu/first-result (with-u1 (create-a 1)))
           bs1 (mapv #(tu/first-result (with-u2 (create-b 1 %))) [10 20])
           bs2 (tu/result (with-u2 (lookup-bs 1)))
           is-bs (fn [bs]
                   (is (= (count bs) 2))
                   (is (every? b? bs))
                   (is (every? #(= #{"u1@i1018.com" "u2@i1018.com"}
                                   (cn/owners %)) bs)))]
       (is (a? a1))
       (is-bs bs1)
       (is-bs bs2)))))
