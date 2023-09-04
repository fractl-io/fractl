(ns fractl.test.features03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.util.seq :as su]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            [fractl.lang.internal :as li]
            [fractl.lang.syntax :as ls]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-845-all-dataflows
  (defcomponent :I845
    (entity :I845/E {:X {:type :Int :indexed true}})
    (event :I845/Event01 {:E :Int})
    (dataflow
     :I845/Event01
     {:I845/E {:X? :I845/Event01.E} :as :E}
     [:delete :I845/E {:X 100}])
    (dataflow
     :I845/Event02
     {:I845/E {:X? [:> :I845/Event02.E]}}))
  (let [dfs (cn/all-dataflows :I845)
        event-names (set (mapv first dfs))
        expected-event-names #{:I845/Event02 :I845/Event01
                               :I845/Lookup_E :I845/Create_E
                               :I845/Update_E :I845/Delete_E}
        df-obj? (fn [x]
                  (and
                   (map? x)
                   (= (set (keys x))
                      #{:head :event-pattern :patterns :opcode})))
        df01 (first (filter #(= :I845/Event01 (first %)) dfs))]
    (is (= [{:I845/E {:X? :I845/Event01.E}, :as :E} [:delete :I845/E {:X 100}]]
           (:patterns (second df01))))
    (is (= expected-event-names (set/intersection expected-event-names event-names)))
    (every? #(df-obj? (second %)) dfs)))

(deftest issue-846-remove-records
  (defcomponent :I846
    (entity
     :I846/E
     {:X {:type :Int :indexed true}})
    (record
     :I846/R
     {:A :Int}))
  (let [evts (cn/all-crud-events :I846/E)]
    (is (cn/fetch-entity-schema :I846/E))
    (is (cn/fetch-meta :I846/E))
    (is (su/all-true? (mapv cn/fetch-event-schema evts)))
    (is (su/all-true? (mapv cn/fetch-event-schema evts)))
    (is (cn/fetch-schema :I846/R))
    (is (cn/fetch-meta :I846/R))
    (let [c (cn/remove-entity :I846/E)]
      (is c)
      (is (not (cn/fetch-entity-schema :I846/E)))
      (is (not (cn/fetch-meta :I846/E)))
      (is (every? nil? (mapv cn/fetch-event-schema evts)))
      (is (cn/fetch-schema :I846/R))
      (is (cn/fetch-meta :I846/R)))
    (let [c (cn/remove-record :I846/R)]
      (is c)
      (is (not (cn/fetch-entity-schema :I846/E)))
      (is (not (cn/fetch-meta :I846/E)))
      (is (every? nil? (mapv cn/fetch-event-schema evts)))
      (is (not (cn/fetch-schema :I846/R)))
      (is (not (cn/fetch-meta :I846/R))))))

(deftest unqualified-name
  (is (= :E (ls/unqualified-name :C/E)))
  (is (= :E (ls/unqualified-name [:C :E])))
  (is (= :E (ls/unqualified-name :E)))
  (is (not (ls/unqualified-name "abc"))))

(deftest is-fully-qualified
  (is (not (ls/fully-qualified? :Hello)))
  (is (ls/fully-qualified? :Acme.Core/Employee))
  (is (ls/fully-qualified? :Acme :Acme.Core/Employee))
  (is (not (ls/fully-qualified? :Abc :Acme.Core/Employee)))
  (is (not (ls/fully-qualified? :Acme.Core))))

(deftest name-info
  (is (= (ls/name-info :Hello) {:record :Hello}))
  (is (= (ls/name-info :Acme.Core)
         {:model :Acme, :component :Core, :record nil}))
  (is (= (ls/name-info :Acme.Core :Acme.Core.Abc)
         {:model :Acme.Core, :component :Abc, :record nil}))
  (is (= (ls/name-info :Acme/Hello)
         {:component :Acme :record :Hello}))
  (is (= (ls/name-info :Acme.Core/Hello)
         {:model :Acme :component :Core :record :Hello}))
  (is (= (ls/name-info :Acme.Core.Abc/Hello)
         {:model :Acme :component :Core.Abc :record :Hello}))
  (is (= (ls/name-info :Acme :Acme.Core.Abc/Hello)
         {:model :Acme :component :Core.Abc :record :Hello}))
  (is (= (ls/name-info :Acme.Core :Acme.Core.Abc/Hello)
         {:model :Acme.Core :component :Abc :record :Hello}))
  (is (not (ls/name-info :Xyz :Acme.Core/Hello))))

(deftest from-with-query-update
  (defcomponent :Ft
    (entity
     :Ft/E
     {:Id {:type :Int :identity true}
      :Y :Int
      :X :Int}))
  (let [e1 (tu/first-result {:Ft/Create_E
                             {:Instance
                              {:Ft/E {:Id 1 :X 100 :Y 200}}}})]
    (is (cn/instance-of? :Ft/E e1))
    (is (= 100 (:X e1)))
    (let [e2 (tu/first-result {:Ft/Update_E {:Id 1 :Data {:X 300}}})]
      (is (cn/instance-eq? e1 e2))
      (is (= 300 (:X e2))))))

(deftest issue-962-recursive-contains
  (defcomponent :I962
    (entity
     :I962/Employee
     {:Name {:type :String
             :identity true}})
    (relationship
     :I962/WorksFor
     {:meta {:contains [:I962/Employee :I962/Employee]}}))
  (let [e1 (tu/first-result
            {:I962/Create_Employee
             {:Instance
              {:I962/Employee
               {:Name "e01"}}}})
        e2 (tu/first-result
            {:I962/Create_Employee
             {:Instance
              {:I962/Employee
               {:Name "e02"}}
              li/path-attr (str "/Employee/" (li/id-attr e1) "/WorksFor")}})
        e? (partial cn/instance-of? :I962/Employee)
        lookup-e (fn [e]
                   (tu/first-result
                    {:I962/Lookup_Employee
                     {li/path-attr (li/path-attr e)}}))]
    (is (e? e1))
    (is (li/null-path? (li/path-attr e1)))
    (is (e? e2))
    (is (pos? (s/index-of (li/path-attr e2) (li/id-attr e1))))
    (is (cn/same-instance? e1 (lookup-e e1)))
    (is (cn/same-instance? e2 (lookup-e e2)))))

(deftest generic__id__access
  (defcomponent :Gid
    (entity
     :Gid/E
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Gid/F
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :Gid/G
     {:Id {:type :Int :identity true}
      :Z :Int})
    (relationship
     :Gid/R1
     {:meta {:contains [:Gid/E :Gid/F]}})
    (relationship
     :Gid/R2
     {:meta {:between [:Gid/F :Gid/G]}})
    (dataflow
     :Gid/MakeR2
     {:Gid/F {li/path-attr? :Gid/MakeR2.FPath} :as [:F]}
     {:Gid/G {:Id? :Gid/MakeR2.GId} :as [:G]}
     ;; __Id__ generically refers to the identity attribute.
     {:Gid/R2 {:F :F.__Id__ :G :G.__Id__}}))
  (let [e (tu/first-result
           {:Gid/Create_E
            {:Instance {:Gid/E {:Id 1 :X 10}}}})
        f (tu/first-result
           {:Gid/Create_F
            {:Instance {:Gid/F {:Id 2 :Y 20}}
             li/path-attr "/E/1/R1"}})
        g (tu/first-result
           {:Gid/Create_G
            {:Instance {:Gid/G {:Id 3 :Z 30}}}})]
    (is (cn/instance-of? :Gid/E e))
    (is (cn/instance-of? :Gid/F f))
    (is (cn/instance-of? :Gid/G g))
    (let [r2 (tu/first-result
              {:Gid/MakeR2 {:FPath (li/path-attr f) :GId (:Id g)}})]
      (is (cn/instance-of? :Gid/R2 r2))
      (is (= (:G r2) (:Id g)))
      (is (= (:F r2) (li/id-attr f))))))

(deftest issue-974
  (defcomponent :I974
    (entity
     :I974/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :I974/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :I974/C
     {:Id {:type :Int :identity true}
      :Z :Int})
    (entity
     :I974/D
     {:Id {:type :Int :identity true}
      :S :Int})
    (relationship
     :I974/R1
     {:meta {:contains [:I974/A :I974/B]}})
    (relationship
     {:I974/R2
      {:meta {:contains [:I974/B :I974/C]}}})
    (relationship
     :I974/R3
     {:meta {:between [:I974/C :I974/D]}
      :R :Int})
    (relationship
     :I974/R4
     {:meta {:between [:I974/D :I974/D :as [:I :J]]}
      :T :Int})
    (dataflow
     :I974/CreateB
     {:I974/B
      {:Id :I974/CreateB.Id :Y '(* :I974/CreateB.Id 20)}
      :-> [[:I974/R1 {:I974/A {:Id? :I974/CreateB.A}}]]})
    (dataflow
     :I974/CreateC
     {:I974/C
      {:Id :I974/CreateC.Id :Z '(* :I974/CreateC.Id 5)}
      :-> [[:I974/R2 {:I974/B? {} :-> [[:I974/R1? {:I974/A {:Id? :I974/CreateC.A}} :I974/CreateC.B]]}]
           [{:I974/R3 {:R :I974/CreateC.R}} {:I974/D {:Id :I974/CreateC.D :S '(* 2 :I974/CreateC.D)}}]]})
    (dataflow
     :I974/FindC
     {:I974/C
      {:Z? :I974/FindC.Z}
      :-> [[:I974/R2? {:I974/B? {} :-> [[:I974/R1? {:I974/A {:Id? :I974/FindC.A}} :I974/FindC.B]]} :I974/FindC.C]
           [{:I974/R3 {:D? :I974/FindC.D}}]]})
    (dataflow
     :I974/CreateD
     {:I974/D
      {:Id :I974/CreateD.Id :S '(* :I974/CreateD.Id 3)}
      :-> [[{:I974/R4 {:T :I974/CreateD.T}} {:I974/D {:Id? :I974/CreateD.J}}]]})
    (dataflow
     :I974/FindD
     {:I974/D? {} :-> [[{:I974/R4 {:J? :I974/FindD.J}}]]}))
  (let [create-a-evt (fn [id] {:I974/Create_A {:Instance {:I974/A {:Id id :X (* id 10)}}}})
        [a1 a2] (mapv #(tu/first-result (create-a-evt %)) [1 2])
        a? (partial cn/instance-of? :I974/A)
        lookup-inst #(tu/first-result {%1 {li/path-attr %2}})]
    (is (every? a? [a1 a2]))
    (let [create-b-evt (fn [a id] {:I974/CreateB {:Id id :A a}})
          b? (partial cn/instance-of? :I974/B)
          b1 (tu/first-result (create-b-evt 1 10))
          lookup-b (partial lookup-inst :I974/Lookup_B)]
      (is b? b1)
      (is (cn/same-instance? b1 (lookup-b "path://I974$A/1/I974$R1/I974$B/10")))
      (let [create-c-evt (fn [a b id] {:I974/CreateC {:Id id :A a :B b :R 464 :D 12}})
            c? (partial cn/instance-of? :I974/C)
            c1 (tu/first-result (create-c-evt 1 10 100))
            lookup-c (partial lookup-inst :I974/Lookup_C)]
        (is c? c1)
        (is (cn/same-instance? c1 (lookup-c "path://I974$A/1/I974$R1/I974$B/10/I974$R2/I974$C/100")))
        (is (cn/same-instance? c1 (tu/first-result {:I974/FindC {:Z 500 :C 100 :A 1 :B 10 :D 12}}))))))
  (let [ds (mapv #(tu/result {:I974/CreateD {:Id % :T (* % 100) :J 12}}) [10 20])
        d? (partial cn/instance-of? :I974/D)
        chk (fn [ds]
              (is (= 2 (count ds)))
              (is (= (set (mapv :Id ds)) #{10 20}))
              (is (every? d? ds)))]
    (chk ds)
    (chk (tu/result {:I974/FindD {:J 12}}))))

(deftest issue-1012-shorter-query-syntax
  (defcomponent :I1012
    (entity
     :I1012/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :I1012/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :I1012/R
     {:meta {:contains [:I1012/A :I1012/B]}})
    (dataflow
     :I1012/LookupAllB
     {:I1012/B? {}
      :-> [[:I1012/R? {:I1012/A {:Id? :I1012/LookupAllB.A}}]]})
    (dataflow
     :I1012/LookupAllBAlias
     {:I1012/A {:Id? :I1012/LookupAllBAlias.A} :as [:A]}
     {:I1012/B? {}
      :-> [[:I1012/R? :A]]})
    (dataflow
     :I1012/LookupAllBOnY
     {:I1012/B {:Y? :I1012/LookupAllBOnY.Y}
      :-> [[:I1012/R? {:I1012/A {:Id? :I1012/LookupAllBOnY.A}}]]})
    (dataflow
     :I1012/LookupAllBOnYAlias
     {:I1012/A {:Id? :I1012/LookupAllBOnYAlias.A} :as [:A]}
     {:I1012/B {:Y? :I1012/LookupAllBOnYAlias.Y}
      :-> [[:I1012/R? :A]]})
    (dataflow
     :I1012/LookupB
     {:I1012/B {:Id? :I1012/LookupB.B}
      :-> [[:I1012/R? {:I1012/A {:Id? :I1012/LookupB.A}}]]})
    (dataflow
     :I1012/LookupBOnY
     {:I1012/B {:Id? :I1012/LookupBOnY.B
                :Y? :I1012/LookupBOnY.Y}
      :-> [[:I1012/R? {:I1012/A {:Id? :I1012/LookupBOnY.A}}]]})
    (dataflow
     :I1012/LookupBAlias
     {:I1012/A {:Id? :I1012/LookupBAlias.A} :as [:A]}
     {:I1012/B {:Id? :I1012/LookupBAlias.B}
      :-> [[:I1012/R? :A]]}))
  (let [as (mapv #(tu/first-result
                   {:I1012/Create_A
                    {:Instance
                     {:I1012/A {:Id % :X (* % 10)}}}})
                 [1 2])
        crb #(tu/first-result
              {:I1012/Create_B
               {:Instance
                {:I1012/B {:Id %2 :Y (* %2 20)}}
                li/path-attr (str "/A/" %1 "/R")}})
        bids [100 200 300]
        bs1 (mapv (partial crb 1) bids)
        bs2 (mapv (partial crb 2) bids)
        b? (partial cn/instance-of? :I1012/B)
        rs (tu/result
            {:I1012/LookupAllBOnY
             {:Y (* 20 100) :A 1}})]
    (is (= 1 (count rs)))
    (is (b? (first rs)))
    (is (= rs (tu/result
               {:I1012/LookupAllBOnYAlias
                {:Y (* 20 100) :A 1}})))
    (let [rs (tu/result
              {:I1012/LookupAllB {:A 1}})]
      (is (= 3 (count rs)))
      (is (every? b? rs)))
    (let [rs (tu/result {:I1012/LookupBAlias {:A 2 :B 200}})]
      (is (= 1 (count rs)))
      (is (b? (first rs)))
      (is (= (li/path-attr (first rs)) "path://I1012$A/2/I1012$R/I1012$B/200"))
      (is (= rs (tu/result {:I1012/LookupB {:A 2 :B 200}})))
      (is (= rs (tu/result {:I1012/LookupBOnY {:A 2 :B 200 :Y (* 20 200)}})))
      (is (tu/not-found? (tu/eval-all-dataflows {:I1012/LookupBOnY {:A 2 :B 200 :Y (* 5 200)}}))))))

(deftest contains-by-path ; issue-1018
  (defcomponent :Cbp
    (entity
     :Cbp/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Cbp/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :Cbp/C
     {:Id {:type :Int :identity true}
      :Z :Int})
    (relationship
     :Cbp/R
     {:meta {:contains [:Cbp/A :Cbp/B]}})
    (dataflow
     :Cbp/MakeB
     {:Cbp/B {:Id 10 :Y 100}
      :-> [[:Cbp/R :_ :Cbp/MakeB.ParentPath]]})
    (dataflow
     :Cbp/FindB
     {:Cbp/B? {} :-> [[:Cbp/R? :_ :Cbp/FindB.ParentPath]]}))
  (let [a (tu/first-result {:Cbp/Create_A {:Instance {:Cbp/A {:Id 1 :X 10}}}})
        b (tu/result {:Cbp/MakeB {:ParentPath "/A/1/R"}})]
    (is (cn/instance-of? :Cbp/A a))
    (is (cn/instance-of? :Cbp/B b))
    (is (cn/same-instance? b (tu/first-result {:Cbp/FindB {:ParentPath (li/path-attr b)}})))))
