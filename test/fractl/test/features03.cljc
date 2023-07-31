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
              :PATH (str "/Employee/" (li/id-attr e1) "/WorksFor")}})
        e? (partial cn/instance-of? :I962/Employee)
        lookup-e (fn [e]
                   (tu/first-result
                    {:I962/Lookup_Employee
                     {:PATH (li/path-attr e)}}))]
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
     {:Gid/F? {} :-> :Gid/MakeR2.FPath :as [:F]}
     {:Gid/G {:Id? :Gid/MakeR2.GId} :as [:G]}
     ;; __Id__ generically refers to the identity attribute.
     {:Gid/R2 {:F :F.__Id__ :G :G.__Id__}}))
  (let [e (tu/first-result
           {:Gid/Create_E
            {:Instance {:Gid/E {:Id 1 :X 10}}}})
        f (tu/first-result
           {:Gid/Create_F
            {:Instance {:Gid/F {:Id 2 :Y 20}}
             :PATH "/E/1/R1"}})
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
