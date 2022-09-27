(ns fractl.test.features02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
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
     {:Name :Kernel/String
      :Salary :Kernel/Decimal})
    (relationship
     :I59401/WorksFor
     {:meta
      {:contains [:I59401/Dept :I59401/Employee]}
      :StartDate {:type :Kernel/DateTime
                  :indexed true}})
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
             (= (first (:refs r2)) cn/id-attr)))))
