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

(deftest issue-917-child-identity
  (defcomponent :I917
    (entity
     :I917/P
     {:A {:type :Int
          :identity true}})
    (entity
     :I917/C
     {:X {:type :Int
          :identity true}
      :Y :Int})
    (entity
     :I917/D
     {:Z {:type :Int
          :identity true}
      :K :Int})
    (relationship
     :I917/R
     {:meta {:contains [:I917/P :I917/C]}})
    (relationship
     :I917/G
     {:meta {:contains [:I917/C :I917/D]}}))
  (let [p? (partial cn/instance-of? :I917/P)
        c? (partial cn/instance-of? :I917/C)
        ps (mapv #(tu/first-result
                   {:I917/Create_P
                    {:Instance
                     {:I917/P
                      {:A %}}}})
                 [1 2])
        c0 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 20 :Y 100}}
              :P 1}})
        c1 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 10 :Y 100}}
              :P 1}})
        c2 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 10 :Y 200}}
              :P 2}})
        c3 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 10 :Y 300}}
              :P 1}})]
    (is (every? p? ps))
    (is (every? c? [c0 c1 c2 c3]))
    (is (= (li/path-attr c0) "/P/1/R/C/20"))
    (is (= (li/path-attr c2) "/P/2/R/C/10"))
    ;; c3 did nothing because of idempotent create
    (is (= 2 (count (tu/result
                     {:I917/LookupAll_C
                      {:P 1}}))))
    (is (= 1 (count (tu/result
                     {:I917/LookupAll_C
                      {:P 2}}))))
    (is (cn/same-instance? c1 (tu/first-result
                               {:I917/Lookup_C
                                {:P 1 :X 10}})))
    (let [d1 (tu/result
              {:I917/Create_D
               {:Instance
                {:I917/D
                 {:Z 101 :K 3}}
                :P 1 :C 20}})
          d2 (tu/eval-all-dataflows
              {:I917/Create_D
               {:Instance
                {:I917/D
                 {:Z 201 :K 4}}
                :P 1 :C 100}})]
      (is (cn/instance-of? :I917/D d1))
      (is (tu/not-found? d2))
      (is (tu/not-found?
           (tu/eval-all-dataflows
            {:I917/Lookup_D
             {:P 1 :C 100 :Z 201}})))
      (is (cn/same-instance?
           d1
           (tu/first-result
            {:I917/Lookup_D
             {:P 1 :C 20 :Z 101}}))))))

(deftest issue-931-contains-patterns
  (defcomponent :I931
    (entity
     :I931/Group
     {:Id :Identity
      :Name {:type :String, :unique true}})
    (entity
     :I931/Member
     {:Email {:type :Email, :identity true}})
    (entity
     :I931/GroupMember
     {:Email {:type :Email, :identity true}
      :IsAdmin :Boolean})
    (relationship
     :I931/MemberGroupMember
     {:meta {:between [:I931/Member :I931/GroupMember]}})
    (relationship
     :I931/Membership
     {:meta {:contains [:I931/Group :I931/GroupMember],
             :cascade-on-delete true}})
    (event
     :I931/CreateGroup
     {:User :Email
      :Name {:type :String}})
    (dataflow
     :I931/CreateGroup
     {:I931/Member {:Email? :I931/CreateGroup.User}, :as [:M]}
     {:I931/Group {:Name :I931/CreateGroup.Name}
      :as :G}
     {:I931/GroupMember {:Email :M.Email, :IsAdmin true},
      :-> [[{:I931/Membership {}} :G] [{:I931/MemberGroupMember {}} :M]]}
     :G))
  (let [m1 (tu/first-result
            {:I931/Create_Member
             {:Instance
              {:I931/Member
               {:Email "m1@i931.org"}}}})
        g1 (tu/eval-all-dataflows
            {:I931/CreateGroup
             {:User "m1@i931.org"
              :Name "g1"}})]
    (println "@@@@@@@@@@@@@@@@@@@@@@@" m1)
    (println "#######################" g1)))
