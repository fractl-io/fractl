(ns fractl.test.features03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.util.seq :as su]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            [fractl.lang.internal :as li]
            [fractl.lang.datetime :as dt]
            [fractl.lang.syntax :as ls]
            [fractl.lang.raw :as raw]
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
                        :on [:Name :No]]
             li/globally-unique true}})
    (relationship
     :I800/WorksFor
     {:meta {:contains [:I800/Department :I800/Employee]
             li/globally-unique true}})
    (relationship
     :I800/AssignedTo
     {:meta {:contains [:I800/Employee :I800/Assignment]
             li/globally-unique true}})
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
                               {:I800/Create_Company
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
          dt (tu/first-result
              {:I800/UpdateDepartment
               {:Company "acme" :Department "101" :Location "B786"}})]
      (is (every? employee? es))
      (is (every? #(apply works-for? %) [["1" e1] ["2" e2] ["3" e3]]))
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
     {:meta {:contains [:I786/T :I786/S]
             li/globally-unique true}})
    (entity
     :I786/A
     {:X {:type :Int :identity true}})
    (relationship
     :I786/R1
     {:meta {:contains [:I786/S :I786/A]
             li/globally-unique true}})
    (entity
     :I786/B
     {:K {:type :Int :default 1}
      :Y {:type :Int :identity true}})
    (relationship
     :I786/R
     {:meta {:contains [:I786/A :I786/B]
             li/globally-unique true}}))
  (let [t (tu/first-result {:I786/Create_T
                            {:Instance
                             {:I786/T {:I 1}}}})
        s (tu/first-result {:I786/Create_S
                            {:Instance
                             {:I786/S {:J 2}}
                             :T 1}})
        a (tu/result {:I786/Create_A
                      {:Instance
                       {:I786/A {:X 100}}
                       :T 1 :S 2}})
        b1 (tu/result {:I786/Create_B
                      {:Instance
                       {:I786/B {:Y 20}}
                       :A 100 :T 1 :S 2}})
        b2 (tu/first-result
            {:I786/Lookup_B
             {:A 100 :Y 20 :T 1 :S 2}})]
    (is (cn/instance-of? :I786/A a))
    (is (cn/instance-of? :I786/B b1))
    (let [r (first (li/rel-tag b1))]
      (is (cn/instance-of? :I786/R r))
      (is (and (= 100 (:A r)) (= 20 (:B r)))))
    (is (cn/same-instance? b2 (dissoc b1 li/rel-tag)))))

(deftest issue-801-upsert-endpoints-for-between-rels
  (defcomponent :I801
    (entity
     :I801/A
     {:X :Int
      :Id {:type :Int :identity true}})
    (entity
     :I801/B
     {:Y :Int
      :Id {:type :Int :identity true}})
    (relationship
     :I801/R1
     {:meta {:between [:I801/A :I801/B]}})
    (relationship
     :I801/R2
     {:meta {:between [:I801/A :I801/A]}})
    (relationship
     :I801/R3
     {:meta {:between [:I801/A :I801/B :on [:X :Y]]}}))
  (let [[a1 a2] (mapv
                 #(tu/first-result
                   {:I801/Create_A
                    {:Instance {:I801/A {:X (* 100 %) :Id %}}}})
                 [1 3])
        b1 (tu/first-result
            {:I801/Create_B
             {:Instance {:I801/B {:Y 200 :Id 10}}}})
        a11 (tu/result
             {:I801/Create_R1
              {:Instance {:I801/R1 {:A 1 :B 10}}}})
        r1 (first a11)]
    (is (cn/instance-of? :I801/R1 r1))
    (is (= (:A r1) 1))
    (is (= (:B r1) 10))
    (let [a11 (tu/result
               {:I801/Create_R2
                {:Instance {:I801/R2 {:A1 1 :A2 3}}}})
          r2 (first a11)]
      (is (cn/instance-of? :I801/R2 r2))
      (is (= (:A1 r2) 1))
      (is (= (:A2 r2) 3)))
    (let [a11 (tu/result
               {:I801/Create_R3
                {:Instance {:I801/R3
                            {:A 300 :B 200
                             :AIdentity 3 :BIdentity 10}}}})
          r3 (first a11)]
      (is (cn/instance-of? :I801/R3 r3))
      (is (= (:A r3) 300))
      (is (= (:B r3) 200))
      (is (= (:AIdentity r3) 3))
      (is (= (:BIdentity r3) 10)))))

(deftest lookup-all
  (defcomponent :LA
    (entity
     :LA/A
     {:X {:type :Int :identity true}})
    (entity
     :LA/B
     {:Y {:type :Int :identity true}})
    (relationship
     :LA/R
     {:meta {:contains [:LA/A :LA/B]
             li/globally-unique true}}))
  (let [as (mapv
            #(tu/first-result
              {:LA/Create_A
               {:Instance
                {:LA/A {:X %}}}})
            [1 2])
        bs_1 (mapv #(tu/result
                     {:LA/Create_B
                      {:Instance
                       {:LA/B {:Y %}}
                       :A 1}})
                   [10 20 30])
        bs_2 (mapv #(tu/result
                     {:LA/Create_B
                      {:Instance
                       {:LA/B {:Y %}}
                       :A 2}})
                   [100 200])]
    (defn- sum [insts attr]
      (reduce (fn [x a] (+ x (attr a))) 0 insts))
    (defn- is-sum-xs [rs]
      (is (= 2 (count rs)))
      (is (= 3 (sum rs :X))))
    (is (every? (partial cn/instance-of? :LA/A) as))
    (is-sum-xs as)
    (defn- check-bs [bs_1 bs_2]
      (is (every? (partial cn/instance-of? :LA/B) bs_1))
      (is (= 3 (count bs_1)))
      (is (= 60 (sum bs_1 :Y)))
      (is (every? (partial cn/instance-of? :LA/B) bs_2))
      (is (= 2 (count bs_2)))
      (is (= 300 (sum bs_2 :Y))))
    (check-bs bs_1 bs_2)
    (let [rs (tu/result {:LA/LookupAll_A {}})]
      (is (every? (partial cn/instance-of? :LA/A) rs))
      (is-sum-xs rs))
    (let [bs_1 (tu/result {:LA/LookupAll_B {:A 1}})
          bs_2 (tu/result {:LA/LookupAll_B {:A 2}})]
      (check-bs bs_1 bs_2))))

(deftest issue-839-entity-names-filter
  (defcomponent :I839
    (entity
     :I839/E1
     {:X :Int})
    (entity
     :I839/E2
     {:Y :Int})
    (relationship
     :I839/R
     {:meta {:between [:I839/E1 :I839/E2]}}))
  (is (= #{:I839/E1 :I839/E2} (set (cn/user-entity-names :I839)))))

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

(deftest issue-846-remove-relationship
  (defcomponent :I846R
    (entity
     :I846R/E1
     {:X :Int})
    (entity
     :I846R/E2
     {:A :Int})
    (entity
     :I846R/E3
     {:B :Int})
    (relationship
     :I846R/R1
     {:meta {:between [:I846R/E1 :I846R/E2]}})
    (relationship
     :I846R/R2
     {:meta {:contains [:I846R/E1 :I846R/E3]
             li/globally-unique true}}))
  (tu/is-error #(cn/remove-entity :I846R/E1))
  (tu/is-error #(cn/remove-entity :I846R/E2))
  (tu/is-error #(cn/remove-entity :I846R/E3))
  (is (cn/remove-relationship :I846R/R1))
  (tu/is-error #(cn/remove-entity :I846R/E1))
  (is (cn/remove-entity :I846R/E2))
  (is (cn/remove-relationship :I846R/R2))
  (is (cn/remove-entity :I846R/E1))
  (is (cn/remove-entity :I846R/E3)))

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

(deftest rel-entity-alias
  (defcomponent :Rea
    (entity
     :Rea/E1
     {:X {:type :Int :identity true}})
    (relationship
     :Rea/R
     {:meta {:between [:Rea/E1 :Rea/E1
                       :as [:A :B]]}}))
  (let [[e11 e22 :as es] (mapv #(tu/first-result
                          {:Rea/Create_E1
                           {:Instance
                            {:Rea/E1 {:X %}}}})
                        [1 2])]
    (is (every? (partial cn/instance-of? :Rea/E1) es))
    (let [r (first
             (tu/result {:Rea/Create_R
                         {:Instance {:Rea/R {:A 1 :B 2}}}}))]
      (is (cn/instance-of? :Rea/R r))
      (is (and (= (:A r) 1) (= (:B r) 2))))))

(deftest between-lookup-all
  (defcomponent :Bla
    (entity
     :Bla/E1
     {:X {:type :Int :identity true}})
    (entity
     :Bla/E2
     {:Y {:type :Int :identity true}})
    (relationship
     :Bla/R1
     {:meta {:between [:Bla/E1 :Bla/E2]}})
    (relationship
     :Bla/R2
     {:meta {:between [:Bla/E1 :Bla/E1
                       :as [:A :B]]}})
    (dataflow
     :Bla/CreateR1
     {:Bla/E1
      {:X? :Bla/CreateR1.E1} :as :E1}
     {:Bla/E2
      {:Y? :Bla/CreateR1.E2}
      :-> [{:Bla/R1 {}} :E1]})
    (dataflow
     :Bla/CreateR2
     {:Bla/E1
      {:X? :Bla/CreateR2.E11} :as :E11}
     {:Bla/E1
      {:X? :Bla/CreateR2.E12}
      :-> [{:Bla/R2 {}} :E11]}))
  (let [[e11 e12] (mapv #(tu/first-result
                          {:Bla/Create_E1
                           {:Instance
                            {:Bla/E1 {:X %}}}})
                        [1 2])
        e21 (tu/first-result
             {:Bla/Create_E2
              {:Instance
               {:Bla/E2 {:Y 100}}}})
        r11 (tu/result
             {:Bla/CreateR1
              {:E1 1 :E2 100}})
        r12 (tu/result
             {:Bla/CreateR1
              {:E1 2 :E2 100}})
        r13 (tu/result
             {:Bla/CreateR1
              {:E1 2 :E2 100}})
        r21 (tu/result
             {:Bla/CreateR2
              {:E11 1 :E12 2}})
        e1? (partial cn/instance-of? :Bla/E1)
        e2? (partial cn/instance-of? :Bla/E2)
        r1? (partial cn/instance-of? :Bla/R1)
        r2? (partial cn/instance-of? :Bla/R2)]
    (is (and (e1? e11) (e1? e12)))
    (is (and (e2? e21) (e2? r11) (e2? r12) (e2? r13)))
    (is (e1? r21))
    (is (r1? (first (ls/rel-tag r11))))
    (is (r2? (first (ls/rel-tag r21))))
    (let [rs? (fn [predic c rs]
                (and (= (count rs) c)
                     (every? predic rs)))
          r1s? (partial rs? r1?)
          r2s? (partial rs? r2?)
          r11s (tu/result {:Bla/Lookup_R1
                           {:E1 1 :E2 100}})
          r12s (tu/result {:Bla/Lookup_R1
                           {:E1 2 :E2 100}})
          r13s (tu/eval-all-dataflows
                {:Bla/Lookup_R1
                 {:E1 1 :E2 200}})
          r21s (tu/result {:Bla/Lookup_R2
                           {:A 1 :B 2}})
          r22s (tu/result {:Bla/Lookup_R2
                           {:A 2 :B 1}})
          all-r1s (tu/result {:Bla/LookupAll_R1 {}})
          all-r2s (tu/result {:Bla/LookupAll_R2 {}})]
      (r1s? 3 all-r1s)
      (r2s? 1 all-r2s)
      (r1s? 1 r11s)
      (r1s? 2 r12s)
      (tu/not-found? r13s)
      (tu/not-found? r21s)
      (r2s? 1 r22s))))

(deftest del-contains-child
  (defcomponent :DC
    (entity
     :DC/G
     {:Id {:type :Int :identity true}})
    (entity
     :DC/P
     {:Id {:type :Int :identity true}})
    (relationship
     :DC/GP
     {:meta {:contains [:DC/G :DC/P]
             li/globally-unique true}})
    (entity
     :DC/C
     {:Id {:type :Int :identity true}
      :X :Int})
    (relationship
     :DC/R
     {:meta {:contains [:DC/P :DC/C]
             li/globally-unique true}}))
  (let [g (tu/first-result
           {:DC/Create_G {:Instance {:DC/G {:Id 0}}}})
        p (tu/first-result
           {:DC/Create_P {:Instance {:DC/P {:Id 1}} :G 0}})
        c (tu/result
           {:DC/Create_C {:Instance {:DC/C {:Id 2 :X 100}} :P 1 :G 0}})]
    (is (cn/instance-of? :DC/C c))
    (is (cn/same-instance? c (tu/first-result {:DC/Lookup_C {:P 1 :Id 2 :G 0}})))
    (let [c (tu/first-result {:DC/Update_C {:G 0 :P 1 :Id 2 :Data {:X 200}}})]
      (is (cn/instance-of? :DC/C c))
      (is (= 200 (:X c)))
      (is (cn/same-instance? c (tu/first-result {:DC/Delete_C {:P 1 :Id 2 :G 0}})))
      (= :not-found (:status (first (tu/eval-all-dataflows {:DC/Lookup_C {:P 1 :Id 2 :G 0}})))))))

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

(deftest path-with-child-components
  (let [parse (partial li/parse-query-path :Acme.Core)
        parts1 (first (parse "path://Company/:LookupEmployee.C/WorksFor/Acme.Erp$Employee/:LookupEmployee.E"))
        parts2 (first (parse "path://Company/:LookupEmployee.C/WorksFor/Employee/:LookupEmployee.E"))]
    (is (= :Acme.Erp/Employee (:child parts1)))
    (is (= :Acme.Core/Employee (:child parts2)))))

(deftest issue-902-component-code
  (component :I902
             {:clj-import '[(:require [clojure.string :as s])]})
  (record :I902/A {:X :Int :Y {:type :String :identity true}})
  (entity :I902/B {:F {:type :Int :identity true} :G {:oneof ["a" "b" "c"]}})
  (dataflow
   :I902/MakeA
   {:I902/B {:F? :I902/MakeA.B} :as :B}
   {:I902/A {:X :B.F :Y '(str :X :B.G)}})
  (entity :I902/C {:K :Float})
  (relationship :I902/R {:meta {:contains [:I902/B :I902/C]
                                li/globally-unique true}
                         :D :DateTime})
  (event :I902/GetC {:B :Int})
  (dataflow
   :I902/GetC
   {:I902/C? {}
    :-> [:I902/R? {:I902/B {:F? :I902/GetC.B}}]})
  (defn- elem-exists? [edn idx elem]
    (= elem (edn idx)))
  (is (not (raw/as-edn :I902.Core)))
  (defn- check-raw [ordered elems]
    (let [edn (raw/as-edn :I902 ordered)
          e? (partial elem-exists? (vec (rest edn)))
          n (count elems)]
      (loop [i 0]
        (when (< i n)
          (is (e? i (elems i)))
          (recur (inc i))))))
  (check-raw
   false
   [['component :I902 {:clj-import '[(:require [clojure.string :as s])]}]
    ['record :I902/A {:X :Int :Y {:type :String :identity true}}]
    ['entity :I902/B {:F {:type :Int :identity true} :G {:oneof ["a" "b" "c"]}}]
    ['dataflow :I902/MakeA
     {:I902/B {:F? :I902/MakeA.B} :as :B}
     {:I902/A {:X :B.F :Y '(str :X :B.G)}}]
    ['entity :I902/C {:K :Float}]
    ['relationship :I902/R
     {:meta {:contains [:I902/B :I902/C]
             li/globally-unique true}
      :rbac {li/owner-exclusive-crud true}
      :D :DateTime}]
    ['event :I902/GetC {:B :Int}]
    ['dataflow :I902/GetC {:I902/C? {} :-> [:I902/R? {:I902/B {:F? :I902/GetC.B}}]}]])
  (check-raw
   true
   [['component :I902 {:clj-import '[(:require [clojure.string :as s])]}]
    ['record :I902/A {:X :Int :Y {:type :String :identity true}}]
    ['entity :I902/B {:F {:type :Int :identity true} :G {:oneof ["a" "b" "c"]}}]
    ['entity :I902/C {:K :Float}]
    ['relationship :I902/R
     {:meta {:contains [:I902/B :I902/C] li/globally-unique true}
      :rbac {li/owner-exclusive-crud true}
      :D :DateTime}]
    ['event :I902/GetC {:B :Int}]
    ['dataflow :I902/MakeA
     {:I902/B {:F? :I902/MakeA.B} :as :B}
     {:I902/A {:X :B.F :Y '(str :X :B.G)}}]
    ['dataflow :I902/GetC {:I902/C? {} :-> [:I902/R? {:I902/B {:F? :I902/GetC.B}}]}]])
  (event :I902/GetC {:K :Int})
  (dataflow
   :I902/GetC
   {:I902/C? {}
    :-> [:I902/R? {:I902/B {:F? :I902/GetC.K}}]})
  (let [edn (vec (take-last 2 (rest (raw/as-edn :I902 false))))]
    (is (= (first edn) '(event :I902/GetC {:K :Int})))
    (is (= (second edn) '(dataflow
                          :I902/GetC
                          {:I902/C? {}
                           :-> [:I902/R? {:I902/B {:F? :I902/GetC.K}}]})))))
