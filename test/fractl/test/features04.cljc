(ns fractl.test.features04
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-926-rbac-ui-apis
  (let [rbac-spec [{:roles ["user" "manager"] :allow :*}
                   {:roles ["guest"] :allow [:read]}]
        ui-spec {:card {:bgcolor :red}}]
    (defcomponent :I926
      (entity
       :I926/E
       {:Id :Identity
        :X :Int
        :meta {:author "rj"}
        :rbac rbac-spec
        :ui ui-spec}))
    (is (= rbac-spec (cn/fetch-rbac-spec :I926/E)))
    (is (= ui-spec (cn/fetch-ui-spec :I926/E)))))

(deftest issue-980-compilation
  (defcomponent :I980
    (entity
     :I980/A
     {:Id {:type :Int tu/guid true}
      :X :Int})
    (entity
     :I980/B
     {:Id {:type :Int tu/guid true}
      :Y :Int})
    (entity
     :I980/C
     {:Id {:type :Int tu/guid true}
      :Z :Int})
    (relationship
     :I980/R1
     {:meta {:contains [:I980/A :I980/B]}})
    (relationship
     :I980/R2
     {:meta {:between [:I980/B :I980/C]}}))
  (apply dataflow :I980/Temp
         (fractl.compiler/parse-relationship-tree
          {:I980/A {:Id 1 :X 10}
           :I980/R1
           [{:I980/B {:Id 2 :Y 20}
             :I980/R2 {:I980/C {:Id 3 :Z 30}}}
            {:I980/B {:Id 4 :Y 40}
             :I980/R2 {:C 3}}]}))
  (is (cn/instance-of? :I980/A (tu/result {:I980/Temp {}})))
  (is (= :I980/Temp (cn/remove-event :I980/Temp)))
  (is (nil? (seq (tu/eval-all-dataflows {:I980/Temp {}})))))

(deftest issue-1051-between-node-names
  (tu/reset-events!)
  (defcomponent :I1051
    (entity
     :I1051/A
     {:Id {:type :Int tu/guid true}
      :X :Int
      :rbac [{:roles ["i1051-user"] :allow [:create]}]})
    (entity
     :I1051/B
     {:Id {:type :Int tu/guid true}
      :Y :Int
      :rbac [{:roles ["i1051-user"] :allow [:create]}]})
    (relationship
     :I1051/R1
     {:meta {:between [:I1051/A :I1051/B]}
      :rbac {:owner :I1051/A}})
    (relationship
     :I1051/R2
     {:meta {:between [:I1051/A :I1051/A :as [:I :J]]}
      :rbac {:owner :J}})
    (relationship
     :I1051/R3
     {:meta {:between [:I1051/A :I1051/B]}
      :rbac {:owner :I1051/A
             :assign {:ownership [:I1051/B :-> :I1051/A]}}})
    (dataflow
     :I1051/InitUsers
     {:Fractl.Kernel.Identity/User
      {:Email "u1@i1051.com"}}
     {:Fractl.Kernel.Identity/User
      {:Email "u2@i1051.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "i1051-user" :Assignee "u2@i1051.com"}}
     {:Fractl.Kernel.Rbac/RoleAssignment
      {:Role "i1051-user" :Assignee "u1@i1051.com"}}))
  (is (tu/finalize-events))
  (is (cn/instance-of?
       :Fractl.Kernel.Rbac/RoleAssignment
       (tu/first-result {:I1051/InitUsers {}})))
  (tu/call-with-rbac
   (fn []
     (let [create-a (fn [with-user id]
                      (tu/first-result
                       (with-user
                         {:I1051/Create_A
                          {:Instance
                           {:I1051/A {:Id id :X (* id 10)}}}})))
           a? (partial cn/instance-of? :I1051/A)
           lookup-a (fn [with-user id]
                      (tu/first-result
                       (with-user
                         {:I1051/Lookup_A
                          {:Id id}})))
           create-b (fn [with-user id]
                      (tu/first-result
                       (with-user
                         {:I1051/Create_B
                          {:Instance
                           {:I1051/B {:Id id :Y (* id 2)}}}})))
           b? (partial cn/instance-of? :I1051/B)
           create-r1 (fn [with-user id1 id2]
                       (tu/first-result
                        (with-user
                          {:I1051/Create_R1
                           {:Instance
                            {:I1051/R1 {:A id1 :B id2}}}})))
           r1? (partial cn/instance-of? :I1051/R1)
           create-r2 (fn [with-user id1 id2]
                       (tu/first-result
                        (with-user
                          {:I1051/Create_R2
                           {:Instance
                            {:I1051/R2 {:I id1 :J id2}}}})))
           r2? (partial cn/instance-of? :I1051/R2)
           create-r3 (fn [with-user id1 id2]
                       (tu/first-result
                        (with-user
                          {:I1051/Create_R3
                           {:Instance
                            {:I1051/R3 {:A id1 :B id2}}}})))
           delete-r3 (fn [with-user id]
                       (tu/first-result
                        (with-user
                          {:I1051/Delete_R3
                           {li/id-attr id}})))
           r3? (partial cn/instance-of? :I1051/R3)
           wu1 (partial tu/with-user "u1@i1051.com")
           wu2 (partial tu/with-user "u2@i1051.com")
           a1 (create-a wu1 1), a2 (create-a wu2 2)
           b1 (create-b wu1 10), b2 (create-b wu2 20)]
       (is (a? a1)) (is (a? a2))
       (is (b? b1)) (is (b? b2))
       (is (not (create-r1 wu1 (:Id a2) (:Id b2))))
       (is (r1? (create-r1 wu1 (:Id a1) (:Id b2))))
       (is (not (create-r2 wu1 (:Id a1) (:Id a2))))
       (is (r2? (create-r2 wu1 (:Id a2) (:Id a1))))
       (is (not (create-r3 wu1 (:Id a2) (:Id b2))))
       (is (= #{"u1@i1051.com"} (cn/owners a1)))
       (let [a1id (:Id a1)
             r3 (create-r3 wu1 a1id (:Id b2))]
         (is (r3? r3))
         (is (= #{"u1@i1051.com" "u2@i1051.com"} (cn/owners (lookup-a wu2 a1id))))
         (is (r3? (delete-r3 wu1 (li/id-attr r3))))
         (is (not (lookup-a wu2 a1id)))
         (is (= #{"u1@i1051.com"} (cn/owners (lookup-a wu1 a1id)))))))))

(deftest issue-1059-crud-events
  (defcomponent :I1059
    (entity :I1059/A {:Id :Identity :X :Int})
    (entity :I1059/B {:Id :Identity :Y :Int})
    (entity :I1059/A1 {:Id :UUID :X :Int})
    (entity :I1059/ALog {:Id :UUID :Tag :String :User :String})
    (entity :I1059/B1 {:Y :Int})
    (dataflow
     [:after :create :I1059/A]
     {:I1059/A1 {:X :Instance.X :Id :Instance.Id}})
    (dataflow
     [:before :create :I1059/A]
     {:I1059/ALog {:Id :Instance.Id :Tag "create" :User :EventContext.User}})
    (dataflow
     [:before :update :I1059/A]
     {:I1059/ALog {:Id :Instance.Id :Tag "update" :User :EventContext.User}})
    (dataflow
     [:before :delete :I1059/A]
     {:I1059/ALog {:Id :Instance.Id :Tag "delete" :User :EventContext.User}})
    (dataflow
     [:after :delete :I1059/A]
     [:delete :I1059/A1 {:Id :Instance.Id}])
    (dataflow
     [:after :create :I1059/B]
     {:I1059/B1 {:Y :Instance.Y}})
    (dataflow
     :I1059/LookupA1
     {:I1059/A1 {:Id? :I1059/LookupA1.Id}})
    (dataflow
     :I1059/LookupALogs
     {:I1059/ALog {:Id? :I1059/LookupALogs.Id}})
    (dataflow
     :I1059/LookupB1
     {:I1059/B1 {:Y? :I1059/LookupB1.Y}})
    (dataflow
     :I1059/E1
     {:I1059/A {:X :I1059/E1.A} :as :A}
     {:I1059/B {:Y :I1059/E1.B}}
     {:I1059/A {:Id? :A.Id :X 200}}))
  (let [a (tu/first-result
           {:I1059/Create_A
            {:Instance
             {:I1059/A {:X 100}}
             :EventContext {:User "abc"}}})
        a? (partial cn/instance-of? :I1059/A)
        lookup-a1 (fn [id]
                    (tu/eval-all-dataflows
                     {:I1059/LookupA1
                      {:Id id}}))
        a1? (partial cn/instance-of? :I1059/A1)
        lookup-alogs (fn [id]
                       (tu/eval-all-dataflows
                        {:I1059/LookupALogs
                         {:Id id}}))
        alog? (partial cn/instance-of? :I1059/ALog)
        lookup-b1 (fn [y]
                    (tu/result
                     {:I1059/LookupB1
                      {:Y y}}))
        b1? (partial cn/instance-of? :I1059/B1)]
    (is (a? a))
    (is (= 100 (:X a)))
    (let [a1s (tu/fresult (lookup-a1 (:Id a)))]
      (is (= 1 (count a1s)))
      (is (a1? (first a1s))))
    (let [a (tu/first-result {:I1059/E1 {:A 1 :B 2 :EventContext {:User "abc"}}})]
      (is (a? a))
      (is (= 200 (:X a)))
      (is (tu/not-found? (lookup-a1 (:Id a))))
      (let [b1s (lookup-b1 2)]
        (is (= 1 (count b1s)))
        (is (b1? (first b1s))))
      (let [alogs (tu/fresult (lookup-alogs (:Id a)))
            ftr (fn [alogs tag user]
                  (filter #(and (= tag (:Tag %))
                                (= user (:User %)))
                          alogs))
            f (partial ftr alogs)]
        (is (= 2 (count alogs)))
        (is (every? alog? alogs))
        (is (= 1 (count (f "create" "abc"))))
        (is (= 1 (count (f "update" "abc"))))
        (is (cn/same-instance? a (tu/first-result
                                  {:I1059/Delete_A
                                   {:Id (:Id a)
                                    :EventContext {:User "xyz"}}})))
        (let [alogs (tu/fresult (lookup-alogs (:Id a)))
              f (partial ftr alogs)]
          (is (= 3 (count alogs)))
          (is (every? alog? alogs))
          (is (= 1 (count (f "create" "abc"))))
          (is (= 1 (count (f "update" "abc"))))
          (is (= 1 (count (f "delete" "xyz"))))
          (is (tu/not-found? (lookup-a1 (:Id a)))))))))

(deftest crud-events-with-rels
  (defcomponent :Cewr
    (entity
     :Cewr/A
     {:Id {:type :Int tu/guid true}
      :X :Int})
    (entity
     :Cewr/B
     {:Id {:type :Int tu/guid true}
      :Y :Int})
    (entity
     :Cewr/C
     {:Id {:type :Int tu/guid true}
      :Z :Int})
    (relationship
     :Cewr/R1
     {:meta {:contains [:Cewr/A :Cewr/B]}})
    (relationship
     :Cewr/R2
     {:meta {:between [:Cewr/B :Cewr/C]}})
    (dataflow
     :Cewr/FindB
     {:Cewr/B {:Id? :Cewr/FindB.Id}})
    (dataflow
     :Cewr/FindR2
     {:Cewr/R2 {:C? :Cewr/FindR2.C}})
    (dataflow
     [:after :create :Cewr/A]
     {:Cewr/B
      {:Id :Instance.Id :Y '(* :Instance.Id 100)}
      :-> [[:Cewr/R1 :Instance]
           [{:Cewr/R2 {}} {:Cewr/C {:Id? :Instance.Id}}]]}))
  (let [create-a (fn [id]
                   (tu/first-result
                    {:Cewr/Create_A
                     {:Instance
                      {:Cewr/A {:Id id :X (* id 2)}}}}))
        create-c (fn [a-id]
                   (tu/first-result
                    {:Cewr/Create_C
                     {:Instance
                      {:Cewr/C {:Id a-id :Z (* 5 a-id)}}}}))
        lookup-b (fn [a-id]
                   (tu/first-result
                    {:Cewr/FindB {:Id a-id}}))
        a? (partial cn/instance-of? :Cewr/A)
        b? (partial cn/instance-of? :Cewr/B)
        c? (partial cn/instance-of? :Cewr/C)
        c (create-c 1)
        a (create-a 1)]
    (is (a? a))
    (is (c? c))
    (let [b (lookup-b 1)]
      (is (b? b))
      (is (= 1 (:Id b)))
      (is (= "path://Cewr$A/1/Cewr$R1/Cewr$B/1" (li/path-attr b)))
      (let [r2 (tu/first-result {:Cewr/FindR2 {:C 1}})]
        (is (cn/instance-of? :Cewr/R2 r2))
        (is (= (li/id-attr b) (:B r2)))
        (is (= 1 (:C r2)))))))

(deftest allow-underscores
  (defcomponent :Aus
    (entity
     :Aus/E__a
     {:X__1 :Int :X__2 :Int})
    (dataflow
     :Aus/UpdateE__a
     {:Aus/E__a
      {:X__1 :Aus/UpdateE__a.X1
       :X__2 :Aus/UpdateE__a.X2
       :__Id__? :Aus/UpdateE__a.Id}}))
  (let [[e1 e2] (mapv #(tu/first-result
                        {:Aus/Create_E__a
                         {:Instance
                          {:Aus/E__a {:X__1 %1 :X__2 %2}}}})
                      [1 2] [3 4])
        e? (partial cn/instance-of? :Aus/E__a)]
    (is (e? e1)) (is (e? e2))
    (is (cn/same-instance? e1 (tu/first-result
                               {:Aus/Lookup_E__a
                                {:__Id__ (:__Id__ e1)}})))
    (let [e11 (tu/first-result {:Aus/UpdateE__a {:X1 100 :X2 200 :Id (:__Id__ e1)}})
          e12 (tu/first-result {:Aus/Lookup_E__a {:__Id__ (:__Id__ e1)}})]
      (is (cn/same-instance? e11 e12))
      (is (= 100 (:X__1 e12)))
      (is (= 200 (:X__2 e12))))
    (is (cn/same-instance? e2 (tu/first-result
                               {:Aus/Lookup_E__a
                                {:__Id__ (:__Id__ e2)}})))))

(deftest issue-713-relationship-refs
  (defn compute-bonus [level base]
    (if (and level base)
      (* base level 0.1)
      0.0))
  (defcomponent :I713
    (entity
     :I713/Employee
     {:Id :Identity
      :Level :Int
      :Base {:expr :I713/EmployeeSalary.Salary.Base}
      :Bonus {:type :Decimal
              :expr '(fractl.test.features04/compute-bonus
                      :Level :I713/EmployeeSalary.Salary.Base
                      #_[:patterns
                       {:I713/Salary? {}
                        :-> [[{:I713/EmployeeSalary {:Employee? :Id}}]]
                        :as [:S]}
                       :S.Base])}})
    (entity
     :I713/Salary
     {:Id :Identity
      :Base :Decimal})
    (relationship
     :I713/EmployeeSalary
     {:meta {:between [:I713/Employee :I713/Salary]
             :one-one true}})
    (dataflow
     :I713/FindSalary
     {:I713/Salary? {}
      :-> [[{:I713/EmployeeSalary {:Employee? :I713/FindSalary.Employee}}]]
      :as [:S]})
    (dataflow
     :I713/AssignSalary
     {:I713/Salary
      {:Base :I713/AssignSalary.Base}
      :-> [[{:I713/EmployeeSalary {}}
            {:I713/Employee {:Id? :I713/AssignSalary.Employee}}]]}))
  (let [e01 (tu/first-result {:I713/Create_Employee
                              {:Instance
                               {:I713/Employee {:Level 2}}}})
        e? (partial cn/instance-of? :I713/Employee)]
    (is (e? e01))
    (let [s01 (tu/first-result {:I713/AssignSalary {:Employee (:Id e01) :Base 1450.5}})
          s? (partial cn/instance-of? :I713/Salary)]
      (is (s? s01))
      (is (cn/same-instance? s01 (tu/first-result {:I713/FindSalary {:Employee (:Id e01)}})))
      (let [e (tu/first-result {:I713/Lookup_Employee {:Id (:Id e01)}})]
        (is (e? e))
        (is (= (:Base e) (:Base s01)))
        (is (= (:Bonus e) (compute-bonus (:Level e) (:Base s01))))))))

(deftest issue-713-multi-level-refs
  (defn add-zs [cs]
    (reduce (fn [x y] (+ x y)) 0 (mapv :Z cs)))
  (defcomponent :I713M
    (entity
     :I713M/A
     {:Id :Identity
      :X :Int
      :Zs {:type :Int
           :expr '(fractl.test.features04/add-zs :I713M/R1.B.R2.C)}})
    (entity :I713M/B {:Id :Identity :Y :Int})
    (entity :I713M/C {:Id :Identity :Z :Int})
    (relationship :I713M/R1 {:meta {:between [:I713M/A :I713M/B], :one-one true}})
    (relationship :I713M/R2 {:meta {:between [:I713M/B :I713M/C]}}))
  (let [a? (partial cn/instance-of? :I713M/A)
        b? (partial cn/instance-of? :I713M/B)
        c? (partial cn/instance-of? :I713M/C)
        a1 (tu/first-result {:I713M/Create_A {:Instance {:I713M/A {:X 1}}}})
        [b1 b2] (mapv #(tu/first-result {:I713M/Create_B {:Instance {:I713M/B {:Y %}}}}) [2 3])
        [c1 c2 c3] (mapv #(tu/first-result {:I713M/Create_C {:Instance {:I713M/C {:Z %}}}}) [10 20 30])]
    (is (a? a1))
    (is (every? b? [b1 b2]))
    (is (every? c? [c1 c2]))
    (let [r1? (partial cn/instance-of? :I713M/R1)
          r2? (partial cn/instance-of? :I713M/R2)
          r11 (tu/first-result {:I713M/Create_R1 {:Instance {:I713M/R1 {:A (:Id a1) :B (:Id b1)}}}})
          a (tu/first-result {:I713M/Lookup_A {:Id (:Id a1)}})]
      (is (r1? r11))
      (is (cn/instance-eq? a1 a))
      (is (zero? (:Zs a)))
      (let [r21 (tu/first-result {:I713M/Create_R2 {:Instance {:I713M/R2 {:B (:Id b1) :C (:Id c1)}}}})
            r22 (tu/first-result {:I713M/Create_R2 {:Instance {:I713M/R2 {:B (:Id b1) :C (:Id c2)}}}})
            r23 (tu/first-result {:I713M/Create_R2 {:Instance {:I713M/R2 {:B (:Id b2) :C (:Id c3)}}}})
            r24 (tu/first-result {:I713M/Create_R2 {:Instance {:I713M/R2 {:B (:Id b2) :C (:Id c2)}}}})]
        (is (every? r2? [r21 r22 r23 r24]))
        (let [a (tu/first-result {:I713M/Lookup_A {:Id (:Id a1)}})]
          (is (cn/instance-eq? a1 a))
          (is (= 30 (:Zs a))))))))

(deftest issue-1211-contains-in-refs
  (defn add-ys [bs]
    (reduce (fn [x y] (+ x y)) 0 (mapv :Y bs)))
  (defcomponent :I1211
    (entity :I1211/A {:Id :Identity
                      :X :Int
                      :Ys {:type :Int
                           :expr '(fractl.test.features04/add-ys :I1211/R.B)}})
    (entity :I1211/B {:Id :Identity :Y :Int})
    (relationship :I1211/R {:meta {:contains [:I1211/A :I1211/B]}})
    (dataflow
     :I1211/MakeB
     {:I1211/B {:Y :I1211/MakeB.Y} :-> [[:I1211/R {:I1211/A {:Id? :I1211/MakeB.A}}]]}))
  (let [a? (partial cn/instance-of? :I1211/A)
        b? (partial cn/instance-of? :I1211/B)
        [a1 a2 :as aa] (mapv #(tu/first-result {:I1211/Create_A {:Instance {:I1211/A {:X %}}}}) [1 2])]
    (is (every? a? aa))
    (is (cn/same-instance? a1 (tu/first-result {:I1211/Lookup_A {:Id (:Id a1)}})))
    (is (b? (tu/first-result {:I1211/MakeB {:A (:Id a1) :Y 100}})))
    (is (b? (tu/first-result {:I1211/MakeB {:A (:Id a1) :Y 200}})))
    (is (b? (tu/first-result {:I1211/MakeB {:A (:Id a2) :Y 6}})))
    (let [a (tu/first-result {:I1211/Lookup_A {:Id (:Id a1)}})]
      (is (cn/instance-eq? a1 a))
      (is (= 300 (:Ys a))))
    (let [a (tu/first-result {:I1211/Lookup_A {:Id (:Id a2)}})]
      (is (cn/instance-eq? a2 a))
      (is (= 6 (:Ys a))))))
