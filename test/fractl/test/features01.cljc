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


(deftest future-attrs
  (defcomponent :Fa
    (entity
     :Fa/E
     {:X :Kernel/Int
      :Y {:type :Kernel/String
          :future true}})
    (record
     :Fa/R
     {:K :Kernel/String})
    (dataflow
     [:on :Fa/E.Y]
     {:Fa/R {:K :Fa/E.Y}}))
  (let [e (tu/first-result
           {:Fa/Upsert_E
            {:Instance
             {:Fa/E
              {:X 100}}}})]
    (is (cn/instance-of? :Fa/E e))
    (let [r (first (:result (first ((:Y e) "hi"))))]
      (is (cn/instance-of? :Fa/R r))
      (is (= "hi" (:K r))))
    (is (= "hi" ((:Y e))))
    (tu/is-error #((:Y e) 100))
    (is (= "hi" ((:Y e))))
    ((:Y e) "bye")
    (is (= "bye" ((:Y e))))))
