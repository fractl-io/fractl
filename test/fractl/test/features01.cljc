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
