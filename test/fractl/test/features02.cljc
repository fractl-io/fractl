(ns fractl.test.features02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            [fractl.lang.syntax :as ls]
            [fractl.evaluator :as e]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-594-basic-relationships
  (defcomponent :I594B
    (entity
     :I594B/Dept
     {:No {:type :Kernel/Int
           :identity true}
      :Name :Kernel/String})
    (entity
     :I594B/Employee
     {:Name {:type :Kernel/String
             :identity true}
      :Salary :Kernel/Decimal})
    (relationship
     :I594B/WorksFor
     {:meta
      {:contains [:I594B/Dept :I594B/Employee]}
      :Location {:type :Kernel/String :indexed true}})
    (dataflow
     :I594B/CreateEmployee
     {:I594B/Dept {:No? :I594B/CreateEmployee.Dept}
      :as [:D]}
     {:I594B/Employee
      {:Name :I594B/CreateEmployee.Name
       :Salary :I594B/CreateEmployee.Salary}
      :-> [{:I594B/WorksFor {:Location "ddd"}} :D]})
    (dataflow
     :I594B/FindEmployees
     {:I594B/Employee
      {:Salary? [:>= 1300M]}
      :-> [{:I594B/WorksFor {:Location? "ddd"}}
           {:I594B/Dept {:No? :I594B/FindEmployees.Dept}}]})
    (relationship
     :I594B/Spouse
     {:meta
      {:between [:I594B/Employee :I594B/Employee]}}))
  (let [rscm (cn/fetch-schema :I594B/WorksFor)
        r1 (:ref (cn/find-attribute-schema (:Dept rscm)))
        r2 (:ref (cn/find-attribute-schema (:Employee rscm)))
        rels #{:I594B/WorksFor}]
    (is (= rels (cn/find-relationships :I594B/Dept)))
    (is (= (conj rels :I594B/Spouse)
           (cn/find-relationships :I594B/Employee)))
    (is (and (= (:component r1) :I594B)
             (= (:record r1) :Dept)
             (= (first (:refs r1)) :No)))
    (is (and (= (:component r2) :I594B)
             (= (:record r2) :Employee)
             (= (first (:refs r2)) :Name))))
  (let [dept (tu/first-result
              {:I594B/Upsert_Dept
               {:Instance
                {:I594B/Dept
                 {:No 101 :Name "abc"}}}})
        r (tu/result
           {:I594B/CreateEmployee
            {:Name "xyz" :Salary 1300M :Dept 101}})]
    (is (cn/instance-of? :I594B/Dept dept))
    (is (cn/instance-of? :I594B/Employee r))
    (let [rel (first (ls/rel-tag r))]
      (is (cn/instance-of? :I594B/WorksFor rel))
      (is (= (:No dept) (:Dept rel)))
      (is (= (:Name r) (:Employee rel))))
    (let [emps (tu/result
                {:I594B/FindEmployees
                 {:Dept 101}})]
      (is (cn/same-instance? r (first emps))))))

(deftest issue-594-multi-level-relationships
  (defcomponent :I594ML
    (entity
     :I594ML/Company
     {:Name {:type :Kernel/String
             :identity true}})
    (entity
     :I594ML/Dept
     {:Name {:type :Kernel/String
             :identity true}
      :Location :Kernel/String})
    (entity
     :I594ML/Employee
     {:Name {:type :Kernel/String
             :identity true}
      :Salary :Kernel/Decimal})
    (relationship
     :I594ML/PartOf
     {:meta
      {:contains [:I594ML/Company :I594ML/Dept]}})
    (relationship
     :I594ML/WorksFor
     {:meta
      {:contains [:I594ML/Dept :I594ML/Employee]}})
    (dataflow
     :I594ML/CreateDept
     {:I594ML/Company
      {:Name? :I594ML/CreateDept.Company}
      :as [:C]}
     {:I594ML/Dept
      {:Name :I594ML/CreateDept.Dept
       :Location :I594ML/CreateDept.Location}
      :-> [{:I594ML/PartOf {}} :C]})
    (dataflow
     :I594ML/CreateEmployee
     {:I594ML/Dept {:Name? :I594ML/CreateEmployee.Dept}
      :as [:D]}
     {:I594ML/Employee
      {:Name :I594ML/CreateEmployee.Emp
       :Salary :I594ML/CreateEmployee.Salary}
      :-> [{:I594ML/WorksFor {}} :D]})
    (dataflow
     :I594ML/LookupEmployees
     {:I594ML/Employee?
      {}
      :-> [:I594ML/WorksFor?
           {:I594ML/Dept {:Name? :I594ML/LookupEmployees.Dept}
            :-> [:I594ML/PartOf?
                 {:I594ML/Company {:Name? :I594ML/LookupEmployees.Company}}]}]}))
  (let [c (tu/first-result
           {:I594ML/Upsert_Company
            {:Instance
             {:I594ML/Company {:Name "acme"}}}})
        d (tu/result
           {:I594ML/CreateDept
            {:Company "acme"
             :Dept "101"
             :Location "west"}})
        e (tu/result
           {:I594ML/CreateEmployee
            {:Dept "101"
             :Emp "steve"
             :Salary 5600}})
        r (tu/first-result
           {:I594ML/LookupEmployees
            {:Company "acme" :Dept "101"}})]
    (is (cn/instance-of? :I594ML/Company c))
    (is (cn/instance-of? :I594ML/Dept d))
    (let [rel (first (ls/rel-tag d))]
      (is (cn/instance-of? :I594ML/PartOf rel))
      (is (= "acme" (:Company rel)))
      (is (= "101" (:Dept rel))))
    (is (cn/instance-of? :I594ML/Employee e))
    (let [rel (first (ls/rel-tag e))]
      (is (cn/instance-of? :I594ML/WorksFor rel))
      (is (= "101" (:Dept rel)))
      (is (= "steve" (:Employee rel))))
    (is (cn/instance-of? :I594ML/Employee r))
    (is (cn/same-instance? e r))))

(deftest issue-594-multi-relationships
  (defcomponent :I594MR
    (entity
     :I594MR/A
     {:X {:type :Kernel/Int
          :identity true}})
    (entity
     :I594MR/B
     {:Y {:type :Kernel/Int
          :identity true}})
    (entity
     :I594MR/C
     {:Z {:type :Kernel/Int
          :identity true}})
    (relationship
     :I594MR/R1
     {:meta
      {:contains [:I594MR/B :I594MR/A]}})
    (relationship
     :I594MR/R2
     {:meta
      {:contains [:I594MR/C :I594MR/A]}})
    (dataflow
     :I594MR/CreateA
     {:I594MR/A
      {:X :I594MR/CreateA.X}
      :-> [[{:I594MR/R1 {}} {:I594MR/B {:Y? :I594MR/CreateA.B}}]
           [{:I594MR/R2 {}} {:I594MR/C {:Z? :I594MR/CreateA.C}}]]})
    (dataflow
     :I594MR/FindA
     {:I594MR/A? {}
      :-> [[:I594MR/R1? {:I594MR/B {:Y? :I594MR/FindA.B}}]
           [:I594MR/R2? {:I594MR/C {:Z? :I594MR/FindA.C}}]]}))
  (let [b (tu/first-result
           {:I594MR/Upsert_B
            {:Instance
             {:I594MR/B {:Y 100}}}})
        c (tu/first-result
           {:I594MR/Upsert_C
            {:Instance
             {:I594MR/C {:Z 200}}}})
        a (tu/result
           {:I594MR/CreateA
            {:X 1 :B 100 :C 200}})
        r (tu/first-result
           {:I594MR/FindA
            {:B 100 :C 200}})]
    (is (cn/instance-of? :I594MR/B b))
    (is (cn/instance-of? :I594MR/C c))
    (is (cn/instance-of? :I594MR/A a))
    (is (= 2 (count (ls/rel-tag a))))
    (every? #(or (cn/instance-of? :I594MR/R1 %)
                 (cn/instance-of? :I594MR/R2 %))
            (ls/rel-tag a))
    (is (cn/same-instance? r a))))
