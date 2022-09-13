(ns fractl.test.features01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.evaluator :as e]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest eval-block
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :EvBlock
     (record
      :EvBlock/R
      {:A :Kernel/Int
       :B :Kernel/Int})
     (entity
      :EvBlock/E
      {:X :Kernel/Int
       :Y {:eval
           {:patterns
            [{:EvBlock/R
              {:A '(+ :EvBlock/E.X 10)
               :B 100}}]}}}))
   (let [e (tu/first-result
            {:EvBlock/Upsert_E
             {:Instance
              {:EvBlock/E
               {:X 5}}}})
         y ((:Y e))]
     (is (cn/instance-of? :EvBlock/E e))
     (is (= 15 (:A y)))
     (is (= 100 (:B y))))))

(deftest custom-identity ; Issue #610
  (defcomponent :CustId
    (entity
     :CustId/E
     {:SeqNo {:type :Kernel/Int
              :unique true
              :identity true}
      :X :Kernel/String}))
  (let [scm (cn/entity-schema :CustId/E)
        id-scm (cn/find-attribute-schema (:SeqNo scm))]
    (is (and (:unique id-scm) (:indexed id-scm)
             (:identity id-scm)))
    (let [e1 (tu/first-result
              {:CustId/Upsert_E
               {:Instance
                {:CustId/E
                 {:SeqNo 1
                  :X "abc"}}}})
          e2 (tu/first-result
              {:CustId/Upsert_E
               {:Instance
                {:CustId/E
                 {:SeqNo 2
                  :X "xyz"}}}})
          r1 (tu/first-result
              {:CustId/Lookup_E
               {:SeqNo 1}})
          r2 (tu/first-result
              {:CustId/Lookup_E
               {:SeqNo 2}})]
      (is (cn/same-instance? e1 r1))
      (is (cn/same-instance? e2 r2))
      (let [d1 (tu/first-result
                {:CustId/Delete_E
                 {:SeqNo 1}})
            r3 (tu/eval-all-dataflows
                {:CustId/Lookup_E
                 {:SeqNo 1}})
            r4 (tu/first-result
                {:CustId/Lookup_E
                 {:SeqNo 2}})]
        (is (= 1 (:SeqNo d1)))
        (is (= :not-found (:status (first r3))))
        (is (= 2 (:SeqNo r4)))))))

(deftest issue-624-instances-from-map
  (defcomponent :I624
    (entity
     :I624/E
     {:X :Kernel/Int
      :Y :Kernel/Int
      :Z {:type :Kernel/Int :default 24}})
    (record
     :I624/R
     {:A :Kernel/Int
      :B :I624/E})
    (dataflow
     :I624/MakeE
     {:I624/E {}
      :from :I624/MakeE.Data
      :as :E}
     {:I624/R {:A '(+ :E.X :E.Y)
               :B :E}})
    (dataflow
     :I624/MakeE2
     {:I624/E {:Z 200 :X 5}
      :from :I624/MakeE2.Data
      :as :E}
     {:I624/R {:A '(+ :E.X :E.Y)
               :B :E}}))
  (let [r (tu/first-result
           {:I624/MakeE {:Data {:X 10 :Y 4}}})]
    (is (cn/instance-of? :I624/R r))
    (is (cn/instance-of? :I624/E (:B r)))
    (is (= 24 (get-in r [:B :Z])))
    (is (= (:A r) 14 (+ (get-in r [:B :X]) (get-in r [:B :Y]))))
    (let [e (tu/first-result
             {:I624/Lookup_E {cn/id-attr (get-in r [:B cn/id-attr])}})]
      (is (and (= (:X e) 10) (= (:Y e) 4) (= (:Z e) 24)))))
  (let [r (tu/first-result
           {:I624/MakeE2 {:Data {:X 10 :Y 4}}})]
    (is (cn/instance-of? :I624/R r))
    (is (cn/instance-of? :I624/E (:B r)))
    (is (= 200 (get-in r [:B :Z])))
    (is (= 5 (get-in r [:B :X])))
    (is (= (:A r) 9 (+ (get-in r [:B :X]) (get-in r [:B :Y]))))
    (let [e (tu/first-result
             {:I624/Lookup_E {cn/id-attr (get-in r [:B cn/id-attr])}})]
      (is (and (= (:X e) 5) (= (:Y e) 4) (= (:Z e) 200))))))

(deftest issue-625-upsert-subtype
  (defcomponent :I625Ups
    (entity
     :I625Ups/P
     {:X {:type :Kernel/Int
          :indexed true}})
    (event
     :I625Ups/MakeP
     {:Data :Kernel/Map})

    (dataflow
     :I625Ups/MakeP
     {:I625Ups/P {} :from :I625Ups/MakeP.Data})

    (entity
     :I625Ups/C
     {:meta {:inherits :I625Ups/P}
      :Y :Kernel/Int})

    (dataflow
     :I625Ups/MakeC
     {:I625Ups/MakeP
      {:Data [:q# {:X 10 :Y 20}]}
      :with-types {:I625Ups/P :I625Ups/C}}))

  (let [c (tu/first-result
           {:I625Ups/MakeC {}})
        p (tu/first-result
           {:I625Ups/MakeP
            {:Data {:X 100}}})]
    (is (cn/instance-of? :I625Ups/C c))
    (is (and (= 10 (:X c)) (= 20 (:Y c))))
    (is (cn/instance-of? :I625Ups/P p))
    (is (= 100 (:X p)))))
