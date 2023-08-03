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

(deftest preproc-relspec
  (defcomponent :Ppr
    (entity
     :Ppr/A
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Ppr/B
     {:Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :Ppr/R1
     {:meta {:contains [:Ppr/A :Ppr/B]}})
    (dataflow
     :Ppr/MakeBs
     {:Ppr/B
      {:Id 1
       :Y 10}
      :-> [[:Ppr/R1 {:Ppr/A {:Id 100 :X 200}}]]}))
  (let [r (tu/eval-all-dataflows {:Ppr/MakeBs {}})]
    (is r)))

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
        c11 (create-c "/A/1/R1/B/10/R2" 100)
        fq (partial li/as-fully-qualified-path :Mlc)]
    (is (every? a? as))
    (is (every? b? [b1 b2]))
    (is (c? c11))
    (is (= "path://Mlc$A/1/Mlc$R1/Mlc$B/10/Mlc$R2/Mlc$C/100"
           (li/path-attr c11)))
    (is (tu/is-error #(create-c "/A/10/R1/B/10/R2" 200)))
    (is (tu/is-error #(create-c "/A/1/R1/B/1000/R2" 200)))
    (let [rs (tu/result
              {:Mlc/LookupAll_B
               {:PATH (fq "path://A/1/R1/B/%")}})]
      (is (= 1 (count rs)))
      (is (b? (first rs))))
    (let [rs (tu/result
              {:Mlc/LookupAll_C
               {:PATH (fq "path://A/1/R1/B/10/R2/C/%")}})]
      (is (= 1 (count rs)))
      (is (c? (first rs))))
    (is (cn/same-instance? (first as) (tu/first-result {:Mlc/Delete_A {:Id 1}})))
    (is (tu/not-found? (tu/eval-all-dataflows
                        {:Mlc/LookupAll_B
                         {:PATH (fq "path://A/1/R1/B/%")}})))
    (is (tu/not-found? (tu/eval-all-dataflows
                        {:Mlc/LookupAll_C
                         {:PATH (fq "path://A/1/R1/B/10/R2/C/%")}})))))

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
    (entity
     :Cblr/D
     {:Id :Identity
      :Z :Int})
    (relationship
     :Cblr/R
     {:meta {:contains [:Cblr/P :Cblr/C]}})
    (relationship
     :Cblr/S
     {:meta {:contains [:Cblr/C :Cblr/D]}})
    (dataflow
     :Cblr/MakeC
     {:Cblr/P {:X :Cblr/MakeC.X} :as :P}
     {:Cblr/C {:Y :Cblr/MakeC.Y} :-> :P})
    (defn cblr-make-p-path [p c]
      (cn/instance-to-full-path :Cblr/C c p))
    (dataflow
     :Cblr/FindC
     {:Cblr/P {:Id? :Cblr/FindC.P} :as [:P]}
     [:eval '(fractl.test.features02/cblr-make-p-path :P :Cblr/FindC.C) :as :P]
     {:Cblr/C {:Y? :Cblr/FindC.Y} :-> :P})
    (dataflow
     :Cblr/MakeD
     {:Cblr/C? {} :-> :Cblr/MakeD.C :as [:C]}
     {:Cblr/D {:Z :Cblr/MakeD.Z} :-> :C}))
  (let [c? (partial cn/instance-of? :Cblr/C)
        p? (partial cn/instance-of? :Cblr/P)
        pid #(second (filter seq (s/split (li/path-query-string (li/path-attr %)) #"/")))
        make-c #(tu/first-result
                 {:Cblr/MakeC {:X %1 :Y %2}})
        c1 (make-c 1 10)
        c2 (make-c 1 20)
        lookup-p #(tu/first-result
                   {:Cblr/Lookup_P {:Id %}})
        lookup-c #(tu/first-result
                   {:Cblr/Lookup_C {li/path-attr %}})
        make-d #(tu/first-result
                 {:Cblr/MakeD {:C %1 :Z %2}})
        d? (partial cn/instance-of? :Cblr/D)
        cpath #(subs % 0 (s/index-of % "/Cblr$S"))]
    (is (c? c1))
    (is (c? c2))
    (let [p (pid c1)
          p1 (lookup-p p)
          cid (:Id c1)]
      (is (p? p1))
      (is (= p (:Id p1)))
      (is (cn/same-instance? c1 (tu/first-result
                                 {:Cblr/FindC
                                  {:P p :C cid :Y 10}}))))
    (is (cn/same-instance? c1 (lookup-c (li/path-attr c1))))
    (let [d1 (make-d (li/path-attr c1) 200)]
      (is (d? d1))
      (is (pos? (s/index-of (li/path-attr d1) "/Cblr$S")))
      (is (cn/same-instance? c1 (lookup-c (cpath (li/path-attr d1))))))))

(deftest purge-delete-cascades
  (defcomponent :Dac
    (entity
     :Dac/P
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Dac/C
     {:Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :Dac/R
     {:meta {:contains [:Dac/P :Dac/C]}})
    (dataflow
     :Dac/PurgeAll
     [:delete :Dac/P :purge]))
  (let [p (tu/first-result
           {:Dac/Create_P
            {:Instance {:Dac/P {:Id 1 :X 10}}}})
        p2 (tu/first-result
            {:Dac/Create_P
             {:Instance {:Dac/P {:Id 2 :X 20}}}})
        cs (mapv #(tu/first-result
                   {:Dac/Create_C
                    {:Instance {:Dac/C {:Id % :Y (* 2 %)}}
                     :PATH "/P/1/R"}})
                 [10 20])
        cs2 (mapv #(tu/first-result
                    {:Dac/Create_C
                     {:Instance {:Dac/C {:Id % :Y (* 2 %)}}
                      :PATH "/P/2/R"}})
                  [10 20])
        fq (partial li/as-fully-qualified-path :Dac)
        allcs (fn [p f chk]
                (let [cs (f
                          {:Dac/LookupAll_C
                           {:PATH (fq (str "path://P/" p "/R/C/%"))}})]
                  (when chk
                    (is (= 2 (count cs)))
                    (is (every? (partial cn/instance-of? :Dac/C) cs))
                    (is (every? #(s/starts-with?
                                  (li/path-attr %)
                                  (fq (str "path://P/" p "/R/C")))
                                cs)))
                  cs))]
    (is (cn/instance-of? :Dac/P p))
    (is (cn/instance-of? :Dac/P p2))
    (is (= 2 (count cs)))
    (is (every? (partial cn/instance-of? :Dac/C) cs))
    (is (= 2 (count cs2)))
    (is (every? (partial cn/instance-of? :Dac/C) cs2))
    (allcs 1 tu/result true)
    (allcs 2 tu/result true)
    (is (cn/same-instance? p (tu/first-result
                              {:Dac/Lookup_P {:Id 1}})))
    (is (cn/same-instance? p (tu/first-result
                              {:Dac/Delete_P {:Id 1}})))
    (is (tu/not-found? (tu/eval-all-dataflows
                        {:Dac/Lookup_P {:Id 1}})))
    (is (tu/not-found? (allcs 1 tu/eval-all-dataflows false)))
    (is (= :ok (:status (first (tu/eval-all-dataflows {:Dac/PurgeAll {}})))))
    (is (cn/same-instance? p2 (tu/first-result {:Dac/Lookup_P {:Id 2}})))
    (allcs 2 tu/result true)))

(deftest query-by-parent-pattern
  (defcomponent :Qpp
    (entity
     :Qpp/P
     {:Id {:type :Int :identity true}
      :X :Int})
    (entity
     :Qpp/C
     {:Id {:type :Int :identity true}
      :Y :Int})
    (relationship
     :Qpp/R
     {:meta {:contains [:Qpp/P :Qpp/C]}})
    (dataflow
     :Qpp/FindC
     {:Qpp/P {:Id? :Qpp/FindC.P} :as [:P]}
     ;; Also valid -
     ;; 1. {:Qpp/C? {} :-> [[:P]]} = lookup all under :P
     ;; 2. {:Qpp/C? {} :-> [[:P :Qpp/FindC.C]]}, same as below
     {:Qpp/C {:Y? :Qpp/FindC.Y}
      :-> [[:P :Qpp/FindC.C]]}))
  (let [p (tu/first-result
           {:Qpp/Create_P
            {:Instance {:Qpp/P {:Id 1 :X 10}}}})
        c1 (tu/first-result
            {:Qpp/Create_C
             {:Instance {:Qpp/C {:Id 2 :Y 100}}
              :PATH "/P/1/R"}})
        c2 (tu/first-result
            {:Qpp/FindC {:Y 100 :P 1 :C 2}})]
    (is (cn/instance-of? :Qpp/P p))
    (is (cn/same-instance? c1 c2))))

(defn- globally-unique-test [c flag]
  (let [mp (partial li/make-path c)]
    (defcomponent c
      (entity
       (mp :P)
       {:Id {:type :Int :identity true}
        :X :Int})
      (entity
       (mp :C)
       {:Id {:type :Int :identity true}
        :Y :Int})
      (relationship
       (mp :R)
       {:meta {:contains [(mp :P) (mp :C)]
               :globally-unique flag}}))
    (let [create-p #(tu/first-result
                     {(mp :Create_P)
                      {:Instance
                       {(mp :P) {:Id % :X (* 10 %)}}}})
          [p1 p2] (mapv create-p [1 2])
          create-c #(tu/first-result
                     {(mp :Create_C)
                      {:Instance
                       {(mp :C) {:Id %2 :Y (* 100 %2)}}
                       :PATH (str "/P/" %1 "/R")}})
          c1 (create-c 1 2)
          c2 (create-c 2 2)
          c3 (create-c 2 3)
          p? (partial cn/instance-of? (mp :P))
          c? (partial cn/instance-of? (mp :C))
          fq (partial li/as-fully-qualified-path c)
          lookup-c #(tu/eval-all-dataflows
                     {(mp :Lookup_C)
                      {:PATH (fq (str "path://P/" %1 "/R/C/" %2))}})]
      (is (every? p? [p1 p2]))
      (is (every? c? [c1 c2 c3]))
      (is (cn/same-instance? c1 (tu/ffresult (lookup-c 1 2))))
      (if flag
        (is (tu/not-found? (lookup-c 2 2)))
        (is (cn/same-instance? c2 (tu/ffresult (lookup-c 2 2)))))
      (is (cn/same-instance? c3 (tu/ffresult (lookup-c 2 3)))))))

(deftest issue-961-globally-unique
  (globally-unique-test :I961A true)
  (globally-unique-test :I961B false))
