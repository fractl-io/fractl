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

(deftest issue-594-relationship-01
  (defcomponent :I59401
    (entity
     :I59401/Dept
     {:No {:type :Kernel/Int
           :identity true}
      :Name :Kernel/String})
    (entity
     :I59401/Employee
     {:Name {:type :Kernel/String
             :identity true}
      :Salary :Kernel/Decimal})
    (relationship
     :I59401/WorksFor
     {:meta
      {:contains [:I59401/Dept :I59401/Employee]}
      :Location {:type :Kernel/String :indexed true}})
    (dataflow
     :I59401/CreateEmployee
     {:I59401/Dept {:No? :I59401/CreateEmployee.Dept}
      :as [:D]}
     {:I59401/Employee
      {:Name :I59401/CreateEmployee.Name
       :Salary :I59401/CreateEmployee.Salary}
      :-> [{:I59401/WorksFor {:Location "ddd"}} :D]})
    (dataflow
     :I59401/FindEmployees
     {:I59401/Employee
      {:Salary? [:>= 1300M]}
      :-> [{:I59401/WorksFor {:Location? "ddd"}}
           {:I59401/Dept {:No? :I59401/FindEmployees.Dept}}]})
    (relationship
     :I59401/Spouse
     {:meta
      {:between [:I59401/Employee :I59401/Employee]}}))
  (let [rscm (cn/fetch-schema :I59401/WorksFor)
        r1 (:ref (cn/find-attribute-schema (:Dept rscm)))
        r2 (:ref (cn/find-attribute-schema (:Employee rscm)))
        rels #{:I59401/WorksFor}]
    (is (= rels (cn/find-relationships :I59401/Dept)))
    (is (= (conj rels :I59401/Spouse)
           (cn/find-relationships :I59401/Employee)))
    (is (and (= (:component r1) :I59401)
             (= (:record r1) :Dept)
             (= (first (:refs r1)) :No)))
    (is (and (= (:component r2) :I59401)
             (= (:record r2) :Employee)
             (= (first (:refs r2)) :Name))))
  (let [dept (tu/first-result
              {:I59401/Upsert_Dept
               {:Instance
                {:I59401/Dept
                 {:No 101 :Name "abc"}}}})
        r (tu/result
           {:I59401/CreateEmployee
            {:Name "xyz" :Salary 1300M :Dept 101}})]
    (is (cn/instance-of? :I59401/Dept dept))
    (is (cn/instance-of? :I59401/Employee r))
    (is (cn/instance-of? :I59401/WorksFor (ls/rel-tag r)))
    (is (= (:No dept) (:Dept (ls/rel-tag r))))
    (is (= (:Name r) (:Employee (ls/rel-tag r)))))
  (let [emps (tu/result
              {:I59401/FindEmployees
               {:Dept 101}})]
    (println emps)))
