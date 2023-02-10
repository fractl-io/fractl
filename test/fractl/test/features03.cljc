(ns fractl.test.features03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            [fractl.lang.datetime :as dt]
            [fractl.evaluator :as e]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-800-rel-path
  (defcomponent :I800
    (entity
     :I800/Company
     {:Name {:type :String
             :identity true}})
    (entity
     :I800/Department
     {:Id {:type :String
           :identity true}
      :No {:type :String
           :indexed true}
      :Location :String})
    (entity
     :I800/Employee
     {:Email {:type :Email
              :identity true}
      :Name :String})
    (entity
     :I800/Assignment
     {:No {:type :Int :identity true}
      :AssignedOn {:type :DateTime :default dt/now}})
    (relationship
     :I800/Section
     {:meta {:contains [:I800/Company :I800/Department
                        :on [:Name :No]]}})
    (relationship
     :I800/WorksFor
     {:meta {:contains [:I800/Department :I800/Employee]}})
    (relationship
     :I800/AssignedTo
     {:meta {:contains [:I800/Employee :I800/Assignment]}})
    (dataflow
     :I800/CreateDepartment
     {:I800/Department
      {:Id :I800/CreateDepartment.Id
       :No :I800/CreateDepartment.No
       :Location :I800/CreateDepartment.Location}
      :-> [{:I800/Section {}}
           {:I800/Company
            {:Name? :I800/CreateDepartment.Company}}]})
    (dataflow
     :I800/UpdateDepartment
     {:I800/Department
      {:? "path://Company/:UpdateDepartment.Company/Section/Department/:UpdateDepartment.Department"
       :Location :I800/UpdateDepartment.Location}})
    (dataflow
     :I800/CreateEmployee
     {:I800/Employee
      {:Email :I800/CreateEmployee.Email
       :Name :I800/CreateEmployee.Name}
      :-> [{:I800/WorksFor {}}
           {:I800/Department? "path://Company/:CreateEmployee.Company/Section/Department/:CreateEmployee.Department"}]}))
  (let [[c1 c2 :as cs] (mapv #(tu/first-result
                               {:I800/Upsert_Company
                                {:Instance
                                 {:I800/Company
                                  {:Name %}}}})
                             ["acme" "zigma"])
        dept-nos ["101" "102" "101"]
        co-names ["acme" "zigma" "zigma"]
        locs ["A111" "A121" "B089"]
        [d1 d2 d3 :as ds] (mapv #(tu/result
                                  {:I800/CreateDepartment
                                   {:Id %1 :No %2 :Location %3 :Company %4}})
                                ["1" "2" "3"] dept-nos locs co-names)
        company? (partial cn/instance-of? :I800/Company)
        dept? (partial cn/instance-of? :I800/Department)
        section-of? #(= %1 (:Company (first (:-> %2))))]
    (is (every? company? cs))
    (is (every? dept? ds))
    (is (every? #(apply section-of? %) [["acme" d1] ["zigma" d2] ["zigma" d3]]))
    (let [emp-names ["a" "b" "c"]
          [e1 e2 e3 :as es] (mapv #(tu/result
                                    {:I800/CreateEmployee
                                     {:Company %1
                                      :Department %2
                                      :Email (str %3 "@" %1 ".com")
                                      :Name %3}})
                                  co-names dept-nos emp-names)
          employee? (partial cn/instance-of? :I800/Employee)
          works-for? #(= %1 (:Department (first (:-> %2))))
          {df :from dt :to} (:transition
                               (tu/first-result
                                {:I800/UpdateDepartment
                                 {:Company "acme" :Department "101" :Location "B786"}}))]
      (is (every? employee? es))
      (is (every? #(apply works-for? %) [["1" e1] ["2" e2] ["3" e3]]))
      (is (cn/same-instance? df d1))
      (is (and (= (:Id dt) (:Id d1)) (= (:Location dt) "B786"))))))
