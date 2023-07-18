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
    (let [fq (partial li/as-fully-qualified-path :Bcr)
          d1 (tu/first-result
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
          e? (partial cn/instance-of? :Bcr/Employee)]
      (is (d? d1)) (is (every? e? es))
      (defn- lookup-e [e]
        (is (cn/same-instance?
             e (tu/first-result
                {:Bcr/Lookup_Employee
                 {:PATH
                  (fq (str "path://Department/d1/WorksFor/Employee/" (:Email e)))}}))))
      (doseq [e es] (lookup-e e))
      (defn- lookup-all-es [dept cnt es]
        (let [result (first
                      (tu/eval-all-dataflows
                       {:Bcr/LookupAll_Employee
                        {:PATH (fq (str "path://Department/" dept "/WorksFor/Employee/%"))}}))
              rs (when (= :ok (:status result)) (:result result))]
          (if (zero? cnt)
            (is (tu/not-found? result))
            (do (is (= (count rs) cnt))
                (is (every? (fn [e] (some (partial cn/same-instance? e) es)) rs))))))
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
        (lookup-all-es "d1" 1 [e2]))
      (is (d? (tu/first-result {:Bcr/Delete_Department {:Name "d1"}})))
      (lookup-all-es "d1" 0 nil))))

(deftest multi-level-contains
  (defcomponent :Mlc
    (entity
     :Mlc/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Mlc/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :Mlc/C
     {:Id {:type :Int :identity true}
      :Z :Int})
    (relationship
     :Mlc/R1
     {:meta {:contains [:Mlc/A :Mlc/B]}})
    (relationship
     :Mlc/R2
     {:meta {:contains [:Mlc/B :Mlc/C]}}))
  (let [[a1 a1 :as as] (mapv #(tu/first-result
                               {:Mlc/Create_A
                                {:Instance
                                 {:Mlc/A
                                  {:Id % :X (* % 2)}}}})
                             [1 2])
        create-b #(tu/first-result {:Mlc/Create_B
                                    {:Instance
                                     {:Mlc/B
                                      {:Id %2 :Y (* %2 5)}}
                                     :PATH %1}})
        create-c #(tu/first-result {:Mlc/Create_C
                                    {:Instance
                                     {:Mlc/C
                                      {:Id %2 :Z (* %2 10)}}
                                     :PATH %1}})
        a? (partial cn/instance-of? :Mlc/A)
        b? (partial cn/instance-of? :Mlc/B)
        c? (partial cn/instance-of? :Mlc/C)
        b1 (create-b "/A/1/R1" 10)
        b2 (create-b "/A/2/R1" 20)
        c11 (create-c "/A/1/R1/B/10/R2" 100)]
    (is (every? a? as))
    (is (every? b? [b1 b2]))
    (is (c? c11))
    (is (= "path://Mlc$A/1/Mlc$R1/Mlc$B/10/Mlc$R2/Mlc$C/100"
           (li/path-attr c11)))
    (is (tu/is-error #(create-c "/A/10/R1/B/10/R2" 200)))
    (is (tu/is-error #(create-c "/A/1/R1/B/1000/R2" 200)))))

(deftest basic-between-relationships
  (defcomponent :Bbr
    (entity
     :Bbr/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Bbr/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :Bbr/R
     {:meta {:between [:Bbr/A :Bbr/B]}
      :Z :Int})
    (dataflow
     :Bbr/LookupB
     {:Bbr/B {:Y? :Bbr/LookupB.Y}
      :-> [{:Bbr/R {:A? :Bbr/LookupB.A}}]}))
  (let [a1 (tu/first-result
            {:Bbr/Create_A
             {:Instance
              {:Bbr/A {:Id 1 :X 100}}}})
        b1 (tu/first-result
            {:Bbr/Create_B
             {:Instance
              {:Bbr/B {:Id 2 :Y 200}}}})
        a? (partial cn/instance-of? :Bbr/A)
        b? (partial cn/instance-of? :Bbr/B)
        r? (partial cn/instance-of? :Bbr/R)]
    (is (a? a1))
    (is (b? b1))
    (let [create-r (fn [a b z]
                     (tu/first-result
                      {:Bbr/Create_R
                       {:Instance
                        {:Bbr/R
                         {:A a :B b :Z z}}}}))]
      (is (r? (create-r 1 2 300)))
      (tu/is-error #(create-r 3 2 400))
      (let [rs (tu/result
                {:Bbr/LookupB {:Y 200 :A 1}})]
        (is (= 1 (count rs)))
        (is (cn/same-instance? b1 (first rs)))))))
