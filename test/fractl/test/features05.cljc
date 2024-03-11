(ns fractl.test.features05
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.env :as env]
            [fractl.lang
             :refer [component event entity relationship dataflow rule]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest rule-basic
  (defcomponent :Rule01
    (entity :Rule01/A {:Id :Identity :X :Int})
    (entity :Rule01/B {:Id :Identity :Y :Int})
    (rule
     :Rule01/R1
     {:Rule01/A {:X 100} :as :A}
     {:Rule01/B {:Y [:<= 200]} :as :B}
     :then
     {:Rule01/Event1 {:A :A.Id}}
     {:Rule01/Event2 {:B :B.Id}})
    (rule
     :Rule01/R2
     {:Rule01/A {:X [:or [:= 0] [:= 100]]} :as :A}
     :then
     {:Rule01/Event3 {:A :A.Id}}
     :priority 10
     :passive
     :category :Rule01.Abc))
  (let [spec (cn/fetch-rule :Rule01/R1)]
    (is (= [{:Rule01/A {:X 100} :as :A}
            {:Rule01/B {:Y [:<= 200]} :as :B}]
           (cn/rule-condition spec)))
    (is (= [{:Rule01/Event1 {:A :A.Id}}
            {:Rule01/Event2 {:B :B.Id}}]
           (cn/rule-consequence spec)))
    (is (cn/rule-has-least-priority? spec))
    (is (not (cn/rule-is-passive? spec)))
    (is (not (cn/rule-category spec))))
  (let [spec (cn/fetch-rule :Rule01/R2)]
    (is (= [{:Rule01/A {:X [:or [:= 0] [:= 100]]} :as :A}]
           (cn/rule-condition spec)))
    (is (= [{:Rule01/Event3 {:A :A.Id}}]
           (cn/rule-consequence spec)))
    (is (= 10 (cn/rule-priority spec)))
    (is (cn/rule-is-passive? spec))
    (is (= :Rule01.Abc (cn/rule-category spec)))))

(deftest rule-fire-01
  (defcomponent :Rf01
    (entity :Rf01/A {:Id :Identity :X :Int})
    (entity :Rf01/B {:Id :Identity :Y :Int :A :UUID})
    (rule
     :Rf01/R1
     {:Rf01/A {:X 100} :as :A}
     :then
     {:Rf01/B {:Y 100 :A :A.Id}}))
  (let [make-a (fn [x]
                 (let [r (first
                          (tu/eval-all-dataflows
                           {:Rf01/Create_A
                            {:Instance
                             {:Rf01/A {:X x}}}}))]
                   [(:env r) (first (:result r))]))
        [[env1 a1] [env2 a2]] (mapv make-a [10 100])
        a? (partial cn/instance-of? :Rf01/A)
        b? (partial cn/instance-of? :Rf01/B)]
    (is (every? a? [a1 a2]))
    (is (nil? (seq (env/rule-futures env1))))
    (is (b? (first (:result (first (deref (first (env/rule-futures env2))))))))
    (let [bs (tu/result {:Rf01/LookupAll_B {}})]
      (is (every? b? bs))
      (is (= 1 (count bs)))
      (is (= (:A (first bs)) (:Id a2)))
      (is (= 100 (:Y (first bs)))))))
