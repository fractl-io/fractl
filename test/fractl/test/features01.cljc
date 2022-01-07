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
  #_(#?(:clj do
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
       (is (= 100 (:B y)))))
  true)
