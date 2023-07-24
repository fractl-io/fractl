(ns fractl.test.features02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
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
          d? (tu/type-check :Bcr/Department)
          e? (tu/type-check :Bcr/Employee)]
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
  (let [as (mapv #(tu/first-result
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
        a? (tu/type-check :Mlc/A)
        b? (tu/type-check :Mlc/B)
        c? (tu/type-check :Mlc/C)
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
        a? (tu/type-check :Bbr/A)
        b? (tu/type-check :Bbr/B)
        r? (tu/type-check :Bbr/R)]
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

(deftest between-and-contains
  (defcomponent :Bac
    (entity
     :Bac/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Bac/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :Bac/C
     {:Id {:type :Int :identity true}
      :Z :Int})
    (relationship
     :Bac/Rc
     {:meta {:contains [:Bac/A :Bac/B]}})
    (relationship
     :Bac/Rb
     {:meta {:between [:Bac/B :Bac/C]}})
    (dataflow
     :Bac/LookupB
     {:Bac/B? {}
      :-> [:Bac/LookupB.Rc
           {:Bac/Rb {:C? :Bac/LookupB.C}}]}))
  (let [create-a #(tu/first-result
                   {:Bac/Create_A
                    {:Instance
                     {:Bac/A {:Id % :X (* % 2)}}}})
        mkpath #(str "/A/" % "/Rc")
        create-b #(tu/first-result
                   {:Bac/Create_B
                    {:Instance
                     {:Bac/B {:Id %2 :Y (* %2 5)}}
                     :PATH (mkpath %1)}})
        create-c #(tu/first-result
                   {:Bac/Create_C
                    {:Instance
                     {:Bac/C {:Id % :Z (* % 10)}}}})
        create-rb #(tu/first-result
                    {:Bac/Create_Rb
                     {:Instance
                      {:Bac/Rb {:B %1 :C %2}}}})
        a? (tu/type-check :Bac/A)
        b? (tu/type-check :Bac/B)
        c? (tu/type-check :Bac/C)
        rb? (tu/type-check :Bac/Rb)
        as (mapv create-a [1 2 3])
        [b1 b2 b3 :as bs] (mapv create-b [1 2 3] [4 5 4])
        cs (mapv create-c [7 8 9])
        fq (partial li/as-fully-qualified-path :Bac)
        rbs (mapv create-rb [(li/id-attr b1) (li/id-attr b3) (li/id-attr b1)] [7 7 9])]
    (is (every? a? as))
    (is (every? b? bs))
    (is (every? c? cs))
    (is (every? rb? rbs))
    (is (cn/same-instance? b1 (tu/first-result
                               {:Bac/LookupB
                                {:B 4 :Rc (fq "/A/1/Rc/B/4") :C 7}})))))

(deftest multi-contains
  (defcomponent :Mcs
    (entity
     :Mcs/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Mcs/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (entity
     :Mcs/C
     {:Id {:type :Int :identity true}
      :Z :Int})
    (relationship
     :Mcs/R1
     {:meta {:contains [:Mcs/A :Mcs/C]}})
    (relationship
     :Mcs/R2
     {:meta {:contains [:Mcs/B :Mcs/C]}}))
  (let [a1 (tu/first-result
            {:Mcs/Create_A
             {:Instance
              {:Mcs/A
               {:Id 1 :X 2}}}})
        b1 (tu/first-result
            {:Mcs/Create_B
             {:Instance
              {:Mcs/B
               {:Id 2 :Y 20}}}})
        create-c #(tu/first-result {:Mcs/Create_C
                                    {:Instance
                                     {:Mcs/C
                                      {:Id %2 :Z (* %2 10)}}
                                     :PATH %1}})
        a? (tu/type-check :Mcs/A)
        b? (tu/type-check :Mcs/B)
        c? (tu/type-check :Mcs/C)
        c1 (create-c "/A/1/R1" 10)
        c2 (create-c "/B/2/R2" 100)]
    (is (a? a1)) (is (b? b1))
    (is (every? c? [c1 c2]))
    (is (= "path://Mcs$A/1/Mcs$R1/Mcs$C/10" (:PATH c1)))
    (is (= "path://Mcs$B/2/Mcs$R2/Mcs$C/100" (:PATH c2)))
    (is (tu/is-error #(create-c "/A/1/R2" 20)))
    (is (tu/is-error #(create-c "/B/1/R2" 200)))))

(deftest contains-by-local-ref
  (defcomponent :Cblr
    (entity
     :Cblr/P
     {:Id :Identity
      :X :Int})
    (entity
     :Cblr/C
     {:Id :Identity
      :Y :Int})
    (relationship
     :Cblr/R
     {:meta {:contains [:Cblr/P :Cblr/C]}})
    (dataflow
     :Cblr/MakeC
     {:Cblr/P {:X :Cblr/MakeC.X} :as :P}
     {:Cblr/C {:Y :Cblr/MakeC.Y} :-> :P}))
  (let [c? (partial cn/instance-of? :Cblr/C)
        p? (partial cn/instance-of? :Cblr/P)
        pid #(second (filter seq (s/split (li/path-query-string (li/path-attr %)) #"/")))
        make-c #(tu/first-result
                 {:Cblr/MakeC {:X %1 :Y %2}})
        c1 (make-c 1 10)
        lookup-p #(tu/first-result
                   {:Cblr/Lookup_P {:Id %}})
        lookup-c #(tu/first-result
                   {:Cblr/Lookup_C {li/path-attr %}})]
    (is (c? c1))
    (let [p (pid c1)
          p1 (lookup-p p)]
      (is (p? p1))
      (is (= p (:Id p1))))
    (is (cn/same-instance? c1 (lookup-c (li/path-attr c1))))))
