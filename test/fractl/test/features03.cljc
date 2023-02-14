(ns fractl.test.features03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            [fractl.lang.internal :as li]
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

(deftest issue-786-auto-upsert-rels
  (defcomponent :I786
    (entity
     :I786/T
     {:I {:type :Int :identity true}})
    (entity
     :I786/S
     {:J {:type :Int :identity true}})
    (relationship
     :I786/R0
     {:meta {:contains [:I786/T :I786/S]}})
    (entity
     :I786/A
     {:X {:type :Int :identity true}})
    (relationship
     :I786/R1
     {:meta {:contains [:I786/S :I786/A]}})
    (entity
     :I786/B
     {:K {:type :Int :default 1}
      :Y {:type :Int :identity true}})
    (relationship
     :I786/R
     {:meta {:contains [:I786/A :I786/B]}}))
  (let [t (tu/first-result {:I786/Upsert_T
                            {:Instance
                             {:I786/T {:I 1}}}})
        s (tu/first-result {:I786/Upsert_S
                            {:Instance
                             {:I786/S {:J 2}}
                             :T 1}})
        a (tu/result {:I786/Upsert_A
                      {:Instance
                       {:I786/A {:X 100}}
                       :T 1 :S 2}})
        b1 (tu/result {:I786/Upsert_B
                      {:Instance
                       {:I786/B {:Y 20}}
                       :A 100 :T 1 :S 2}})
        b2 (tu/first-result
            {:I786/Lookup_B
             {:A 100 :B 20 :T 1 :S 2}})]
    (is (cn/instance-of? :I786/A a))
    (is (cn/instance-of? :I786/B b1))
    (let [r (first (li/rel-tag b1))]
      (is (cn/instance-of? :I786/R r))
      (is (and (= 100 (:A r)) (= 20 (:B r)))))
    (is (cn/same-instance? b2 (dissoc b1 li/rel-tag)))))
