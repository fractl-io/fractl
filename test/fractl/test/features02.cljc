(ns fractl.test.features02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            [fractl.lang.syntax :as ls]
            [fractl.lang.relgraph :as rg]
            [fractl.lang.internal :as li]
            [fractl.evaluator :as e]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-840-raw-attributes
  (defcomponent :I840
    (attribute :I840/K {:type :String})
    (entity
     :I840/E
     {:X :Int
      :Y {:type :String :default "yyyy"}
      :Z :I840/K}))
  (is (= {:X :Int
          :Y {:type :String
              :default "yyyy"}
          :Z :I840/K}
         (cn/fetch-user-schema :I840/E))))

(deftest basic-contains-relationship
  (let [grades ["a" "b"]]
    (defcomponent :Bcr
      (entity
       :Bcr/Employee
       {:Email {:type :Email :identity true}
        :Name :String
        :Grade {:oneof grades}})
      (entity
       :Bcr/Department
       {:Name {:type :String :identity true}
        :Location {:oneof ["north" "south" "west" "east"]}})
      (relationship
       :Bcr/WorksFor
       {:meta {:contains [:Bcr/Department :Bcr/Employee]}}))
    (is (cn/parent-via? :Bcr/WorksFor :Bcr/Employee :Bcr/Department))
    (let [d1 (tu/first-result
              {:Bcr/Create_Department
               {:Instance
                {:Bcr/Department
                 {:Name "d1" :Location "south"}}}})
          [e1 e2 :as es] (mapv #(tu/first-result
                                 {:Bcr/Create_Employee
                                  {:Instance
                                   {:Bcr/Employee
                                    {:Email (str % "@bcr.com")
                                     :Name % :Grade (rand-nth grades)}}
                                   :PATH "/Department/d1/WorksFor"}})
                               ["e01" "e02"])
          d? (partial cn/instance-of? :Bcr/Department)
          e? (partial cn/instance-of? :Bcr/Employee)
          fq (partial li/as-fully-qualified-path :Bcr)]
      (is (d? d1)) (is (every? e? es))
      (defn- lookup-e [e]
        (is (cn/same-instance?
             e (tu/first-result
                {:Bcr/Lookup_Employee
                 {:PATH
                  (fq (str "path://Department/d1/WorksFor/Employee/" (:Email e)))}}))))
      (doseq [e es] (lookup-e e))
      (defn- lookup-all-es [dept cnt es]
        (let [rs (tu/result
                  {:Bcr/LookupAll_Employee
                   {:PATH (fq (str "path://Department/" dept "/WorksFor/Employee/%"))}})]
          (is (= (count rs) cnt))
          (is (every? (fn [e] (some (partial cn/same-instance? e) es)) rs))))
      (lookup-all-es "d1" 2 es)
      (let [e (tu/first-result
               {:Bcr/Update_Employee
                {:Data {:Name "e0001"}
                 :PATH (fq "path://Department/d1/WorksFor/Employee/e01@bcr.com")}})]
        (is (= "e0001" (:Name e)))
        (is (= (:Email e1) (:Email e)))
        (lookup-e e)
        (lookup-all-es "d1" 2 [e e2])
        (is (cn/same-instance? e (tu/first-result
                                  {:Bcr/Delete_Employee
                                   {:PATH (fq "path://Department/d1/WorksFor/Employee/e01@bcr.com")}})))
        (lookup-all-es "d1" 1 [e2])))))
