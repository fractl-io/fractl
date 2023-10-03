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
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :I980/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :I980/C
     {:Id {:type :Int :identity true}
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
     {:Id {:type :Int :identity true}
      :X :Int
      :rbac [{:roles ["i1051-user"] :allow [:create]}]})
    (entity
     :I1051/B
     {:Id {:type :Int :identity true}
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
    (entity :I1059/B1 {:Y :Int})
    (dataflow
     [:post :create :I1059/A]
     {:I1059/A1 {:X :Instance.X :Id :Instance.Id}})
    (dataflow
     [:post :delete :I1059/A]
     [:delete :I1059/A1 {:Id :Instance.Id}])
    (dataflow
     [:post :create :I1059/B]
     {:I1059/B1 {:Y :Instance.Y}})
    (dataflow
     :I1059/LookupA1
     {:I1059/A1 {:Id? :I1059/LookupA1.Id}})
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
             {:I1059/A {:X 100}}}})
        a? (partial cn/instance-of? :I1059/A)
        lookup-a1 (fn [id]
                    (tu/eval-all-dataflows
                     {:I1059/LookupA1
                      {:Id id}}))
        a1? (partial cn/instance-of? :I1059/A1)
        lookup-b1 (fn [y]
                    (tu/result
                     {:I1059/LookupB1
                      {:Y y}}))
        b1? (partial cn/instance-of? :I1059/B1)]
    (is (a? a))
    (is (a? (tu/first-result {:I1059/E1 {:A 1 :B 2}})))
    (let [a1s (tu/fresult (lookup-a1 (:Id a)))]
      (is (= 1 (count a1s)))
      (is (a1? (first a1s))))
    (let [b1s (lookup-b1 2)]
      (is (= 1 (count b1s)))
      (is (b1? (first b1s))))
    (is (cn/same-instance? a (tu/first-result {:I1059/Delete_A {:Id (:Id a)}})))
    (is (tu/not-found? (lookup-a1 (:Id a))))))
