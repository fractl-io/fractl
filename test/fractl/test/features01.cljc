(ns fractl.test.features01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow set-attributes!]]
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

(deftest future-attrs
  (defcomponent :Fa
    (entity
     :Fa/E
     {:X :Kernel/Int
      :Y {:type :Kernel/String
          :future true}})
    (record
     :Fa/R
     {:K :Kernel/Any})
    (dataflow
     [:on :update :Fa/E]
     {:Fa/R {:K :Fa/E.Y}}))
  (let [e (tu/first-result
           {:Fa/Upsert_E
            {:Instance
             {:Fa/E
              {:X 100}}}})]
    (is (cn/instance-of? :Fa/E e))
    (let [r (first (set-attributes! e {:Y "hi"}))]
      (is (cn/instance-of? :Fa/R r))
      (is (= "hi" @(:K r))))
    (is (= "hi" @(:Y e)))
    (tu/is-error #(set-attributes! e {:Y 100}))
    (is (= "hi" @(:Y e)))
    (set-attributes! e {:Y "bye"})
    (is (= "bye" @(:Y e)))))

(deftest future-set-pattern
  (defcomponent :Fsp
    (entity
     :Fsp/E
     {:X :Kernel/Int
      :Y {:type :Kernel/Int
          :future true
          :default 0}})
    (dataflow
     :Fsp/Evt
     {:Fsp/E
      {:Id? :Fsp/Evt.EId
       :X 200
       :Y 300}}))
  (let [e1 (tu/first-result
           {:Fsp/Upsert_E
            {:Instance
             {:Fsp/E
              {:X 100}}}})]
    (is (cn/instance-of? :Fsp/E e1))
    (is (= 100 (:X e1)))
    (is (= 0 @(:Y e1)))
    (let [r (tu/first-result
             {:Fsp/Evt
              {:EId (:Id e1)}})
          e2 (get-in r [:transition :to])]
      (is (cn/instance-of? :Fsp/E e2))
      (is (= (:Id e1) (:Id e2)))
      (is (= 200 (:X e2)))
      (is (= 100 (:X e1)))
      (is (= 300 @(:Y e2)))
      (is (= 300 @(:Y e1))))))
