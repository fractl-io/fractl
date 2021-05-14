(ns fractl.test.fixes01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.evaluator :as e]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-195
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I195
     (entity {:I195/E1 {:A :Kernel/Int
                        :B :Kernel/Int
                        :C :Kernel/Int
                        :Y :Kernel/DateTime}})
     (dataflow :I195/K
               {:I195/E1 {:A '(+ 5 :B)
                          :B 10
                          :C '(+ 10 :A)
                          :Y '(fractl.lang.datetime/now)}})
     (entity {:I195/E2 {:Y :Kernel/DateTime}})
     (dataflow :I195/KK {:I195/E2 {:Y '(fractl.lang.datetime/now)}}))
   (let [evt (cn/make-instance :I195/K {})
         r (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I195/E1 r))
     (is (dt/parse-date-time (:Y r)))
     (is (= 10 (:B r)))
     (is (= 15 (:A r)))
     (is (= 25 (:C r))))
   (let [evt (cn/make-instance :I195/KK {})
         r (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I195/E2 r))
     (is (dt/parse-date-time (:Y r))))))

(defn- assert-transition [attr-names from-attr-vals to-attr-vals r]
  (let [t (:transition r)]
    (is t)
    (let [from (:from t), to (:to t)]
      (is (= from-attr-vals (map #(% from) attr-names)))
      (is (= to-attr-vals (map #(% to) attr-names))))))

(deftest issue-196
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I196
     (entity {:I196/E1 {:A :Kernel/Int
                        :B :Kernel/Int
                        :C :Kernel/Int
                        :meta {:unique [:A :C]}}}))
   (let [e01 (cn/make-instance :I196/E1 {:A 10 :B 20 :C 30})
         evt1 (cn/make-instance {:I196/Upsert_E1 {:Instance e01}})
         e02 (cn/make-instance :I196/E1 {:A 10 :B 40 :C 50})
         evt2 (cn/make-instance {:I196/Upsert_E1 {:Instance e02}})
         e03 (cn/make-instance :I196/E1 {:A 20 :B 60 :C 30})
         evt3 (cn/make-instance {:I196/Upsert_E1 {:Instance e03}})
         e04 (cn/make-instance :I196/E1 {:A 20 :B 40 :C 70})
         evt4 (cn/make-instance {:I196/Upsert_E1 {:Instance e04}})
         results (map #(ffirst (tu/fresult (e/eval-all-dataflows %)))
                      [evt1 evt2 evt3 evt4])]
     (is (cn/instance-of?
          :I196/E1
          (first results)))
     (let [a (partial assert-transition [:A :B :C])]
       (a [10 20 30] [10 40 30] (second results))
       (a [10 40 30] [10 60 30] (nth results 2)))
     (is (cn/instance-of? :I196/E1 (nth results 3)))
     (is (= [20 40 70] (map #(% (nth results 3)) [:A :B :C]))))))

(deftest issue-206
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I206
     (entity {:I206/E1 {:A :Kernel/Int
                        :B :Kernel/Int
                        :C :Kernel/Int
                        :meta {:unique [:A :C]}}}))
   (let [e01 (cn/make-instance :I206/E1 {:A 10 :B 20 :C 30})
         evt1 (cn/make-instance {:I206/Upsert_E1 {:Instance e01}})
         e02 (cn/make-instance :I206/E1 {:A 10 :C 50} false)
         evt2 (cn/make-instance {:I206/Upsert_E1 {:Instance e02}})
         e03 (cn/make-instance :I206/E1 {:A 20 :B 60 :C 30})
         evt3 (cn/make-instance {:I206/Upsert_E1 {:Instance e03}})
         results (map #(ffirst (tu/fresult (e/eval-all-dataflows %)))
                      [evt1 evt2 evt3])]
     (is (cn/instance-of?
          :I206/E1
          (first results)))
     (let [a (partial assert-transition [:A :B :C])]
       (a [10 20 30] [10 20 30] (second results))
       (a [10 20 30] [10 60 30] (nth results 2))))))

(deftest issue-185
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I185
     (entity {:I185/E {:X :Kernel/Int}})
     (record {:I185/R {:Y :Kernel/Int}})
     (dataflow [:I185/OnXGt10 :when [:> 10 :I185/E.X]]
               {:I185/R {:Y 100}}))
   (let [e (cn/make-instance {:I185/E {:X 20}})
         evt (cn/make-instance {:I185/Upsert_E {:Instance e}})
         result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I185/E result)))))
