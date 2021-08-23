(ns fractl.test.fixes01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.evaluator :as e]
            [fractl.lang.datetime :as dt]
            [fractl.compiler.rule :as rule]
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
         r (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I195/E1 r))
     (is (dt/parse-date-time (:Y r)))
     (is (= 10 (:B r)))
     (is (= 15 (:A r)))
     (is (= 25 (:C r))))
   (let [evt (cn/make-instance :I195/KK {})
         r (first (tu/fresult (e/eval-all-dataflows evt)))]
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
         results (map #(first (tu/fresult (e/eval-all-dataflows %)))
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
         results (map #(first (tu/fresult (e/eval-all-dataflows %)))
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
     (entity {:I185/E {:X :Kernel/Int :Y :Kernel/Int}})
     (record {:I185/R {:Y :Kernel/Int}})
     (dataflow :I185/UpdateE
               {:I185/E {:Id? :I185/UpdateE.Id
                         :X :I185/UpdateE.X
                         :Y :I185/UpdateE.Y}})
     (dataflow [:I185/OnXGt10 :when [:and
                                     [:> :I185/E.X 10]
                                     [:= :I185/E.Y 200]]]
               {:I185/R {:Y '(* 2 :I185/E.Y)}}))
   (let [e (cn/make-instance {:I185/E {:X 10 :Y 1}})
         evt (cn/make-instance {:I185/Upsert_E {:Instance e}})
         r (tu/fresult (e/eval-all-dataflows evt))
         r1 (first r)
         id (:Id r1)
         evt (cn/make-instance {:I185/UpdateE {:Id id :X 20 :Y 100}})
         r2 (tu/fresult (e/eval-all-dataflows evt))
         r3 (first (tu/embedded-results r2))
         evt (cn/make-instance {:I185/UpdateE {:Id id :X 11 :Y 200}})
         r4 (tu/fresult (e/eval-all-dataflows evt))
         r5 (first (tu/embedded-results r4))
         evt (cn/make-instance {:I185/Lookup_E {:Id id}})
         r6 (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (nil? (tu/embedded-results r)))
     (is (cn/instance-of? :I185/E r1))
     (is (= 10 (:X r1)))
     (is (= 1 (:Y r1)))
     (is (cn/instance-of? :I185/E (first r2)))
     (is (= 20 (:X (first r2))))
     (is (nil? r3))
     (is (cn/instance-of? :I185/E (first r4)))
     (is (= 11 (:X (first r4))))
     (is (= 200 (:Y (first r4))))
     (is (cn/instance-of? :I185/R r5))
     (is (= 400 (:Y r5)))
     (is (cn/instance-of? :I185/E r6))
     (is (= 11 (:X r6)))
     (is (= 200 (:Y r6))))))

(deftest issue-213
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I213
     (entity {:I213/E1 {:X :Kernel/Int}})
     (entity {:I213/E2 {:E1 {:ref :I213/E1.Id}
                        :Y :Kernel/Int}})
     (record {:I213/R {:Y :Kernel/Int :Z :Kernel/Int}})
     (dataflow :I213/UpdateE1
               {:I213/E1 {:Id? :I213/UpdateE1.Id
                          :X :I213/UpdateE1.X}})
     (dataflow :I213/UpdateE2
               {:I213/E2 {:Id? :I213/UpdateE2.Id
                          :Y :I213/UpdateE2.Y}})
     (dataflow [:I213/CrossCond
                :when [:and
                       [:> :I213/E1.X 10]
                       [:= :I213/E2.Y 200]]
                :on :I213/E2
                :where [:= :I213/E2.E1 :I213/E1.Id]]
               {:I213/R {:Y '(* :I213/E2.Y :I213/E1.X) :Z 1}})
     (let [e1 (cn/make-instance {:I213/E1 {:X 10}})
           evt (cn/make-instance {:I213/Upsert_E1 {:Instance e1}})
           r1 (tu/fresult (e/eval-all-dataflows evt))
           e1 (first r1)
           e2 (cn/make-instance {:I213/E2 {:E1 (:Id e1)
                                           :Y 20}})
           evt (cn/make-instance {:I213/Upsert_E2 {:Instance e2}})
           r2 (tu/fresult (e/eval-all-dataflows evt))
           e2 (first r2)
           evt (cn/make-instance {:I213/UpdateE1
                                  {:Id (:Id e1)
                                   :X 20}})
           r3 (tu/fresult (e/eval-all-dataflows evt))
           e3 (first r3)
           evt (cn/make-instance {:I213/UpdateE2
                                  {:Id (:Id e2)
                                   :Y 200}})
           r4 (tu/fresult (e/eval-all-dataflows evt))
           e4 (first r4)
           r5 (first (tu/embedded-results r4))]
       (is (cn/instance-of? :I213/E2 e2))
       (is (nil? (tu/embedded-results r1)))
       (is (nil? (tu/embedded-results r2)))
       (is (nil? (tu/embedded-results r3)))
       (is (= 20 (:X e3)))
       (is (= 200 (:Y e4)))
       (is (cn/instance-of? :I213/R r5))
       (is (= 1 (:Z r5)))
       (is (= 4000 (:Y r5)))))))

(deftest issue-213-no-refs
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I213NR
     (entity {:I213NR/E1 {:X :Kernel/Int
                          :Z {:type :Kernel/Int
                              :indexed true}}})
     (entity {:I213NR/E2 {:Y :Kernel/Int}})
     (record {:I213NR/R {:Y :Kernel/Int}})
     (dataflow [:I213NR/CrossCond
                :when [:and
                       [:> :I213NR/E1.X 10]
                       [:= :I213NR/E2.Y 200]]
                :on :I213NR/E2
                :where [:= :I213NR/E1.Z 1]]
               {:I213NR/R {:Y '(+ :I213NR/E1.X :I213NR/E2.Y)}})
     (let [e1 (cn/make-instance {:I213NR/E1 {:X 9 :Z 2}})
           evt (cn/make-instance {:I213NR/Upsert_E1 {:Instance e1}})
           r1 (tu/fresult (e/eval-all-dataflows evt))
           e1 (first r1)
           e2 (cn/make-instance {:I213NR/E2 {:Y 20}})
           evt (cn/make-instance {:I213NR/Upsert_E2 {:Instance e2}})
           r2 (tu/fresult (e/eval-all-dataflows evt))
           e2 (first r2)
           e11 (cn/make-instance {:I213NR/E1 {:X 11 :Z 1}})
           evt (cn/make-instance {:I213NR/Upsert_E1 {:Instance e11}})
           r11 (tu/fresult (e/eval-all-dataflows evt))
           e11 (first r11)
           e22 (cn/make-instance {:I213NR/E2 {:Y 200}})
           evt (cn/make-instance {:I213NR/Upsert_E2 {:Instance e22}})
           r22 (tu/fresult (e/eval-all-dataflows evt))
           e22 (first r22)
           r (first (tu/embedded-results r22))]
       (is (cn/instance-of? :I213NR/E1 e1))
       (is (nil? (tu/embedded-results r1)))
       (is (cn/instance-of? :I213NR/E2 e2))
       (is (nil? (tu/embedded-results r2)))
       (is (cn/instance-of? :I213NR/R r))
       (is (= 211 (:Y r)))))))

(deftest issue-219-event-context
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I219
     (event :I219/Evt {:Y :Kernel/Int})
     (record :I219/R {:Y :Kernel/Int
                      :Z :Kernel/Map})
     (dataflow :I219/Evt
               {:I219/R {:Y '(+ 10 :I219/Evt.Y)
                         :Z :I219/Evt.EventContext}}))
   (let [ctx {:a 1 :b 2}
         evt (cn/make-instance
              {:I219/Evt
               {:Y 100
                :EventContext ctx}})
         r (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I219/R r))
     (is (= 110 (:Y r)))
     (is (= ctx (:Z r))))))

(deftest issue-231-rules-operators
  (#?(:clj do
      :cljs cljs.core.async/go)
   (let [r1 (rule/compile-rule-pattern [:= 1 :A.B])
         r2 (rule/compile-rule-pattern
             [:and
              [:= "abc" :A.Name]
              [:> :A.Date "2020-01-20"]])
         r3 (rule/compile-rule-pattern
             [:between "2020-01-20" "2021-01-20" :A.Date])
         r4 (rule/compile-rule-pattern
             [:in [1 2 3] :A.B])]
     (is (r1 {:A {:B 1}}))
     (is (not (r1 {:A {:B 2}})))
     (is (r2 {:A {:Name "abc"
                  :Date "2021-01-20"}}))
     (is (r3 {:A {:Date "2021-01-10"}}))
     (is (not (r3 {:A {:Date "2021-02-10"}})))
     (is (r4 {:A {:B 2}}))
     (is (not (r4 {:A {:B 4}}))))))

(deftest issue-241-lowercase-names
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :lcase
     (entity :lcase/e {:x :Kernel/Int})
     (let [e (cn/make-instance
              {:lcase/e {:x 100}})]
       (is (cn/instance-of? :lcase/e e))
       (is (= 100 (:x e)))))))

(deftest issue-314-compound-exprs-in-records
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I314
     (record :I314/R {:X :Kernel/Int
                      :Y '(+ 10 :X)})
     (dataflow :I314/Evt {:I314/R {:X :I314/Evt.X}}))
   (let [r (first
            (tu/fresult
             (e/eval-all-dataflows
              (cn/make-instance
               {:I314/Evt {:X 20}}))))]
     (is (cn/instance-of? :I314/R r))
     (is (= 20 (:X r)))
     (is (= 30 (:Y r))))))

(deftest issue-350-listof
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :LEnt
     (record {:LEnt/E {:X :Kernel/Int}})
     (entity {:LEnt/F {:Es {:listof :LEnt/E}}})
     (event {:LEnt/MakeF {:Xs {:listof :Kernel/Int}}})
     (dataflow
      :LEnt/MakeF
      [:for-each :LEnt/MakeF.Xs
       {:LEnt/E {:X :%}}
       :as :ListofEs]
      {:LEnt/F {:Es :ListofEs}}))
   (let [xs [10 20 30 40]
         evt {:LEnt/MakeF {:Xs xs}}
         result (first (tu/fresult (e/eval-all-dataflows evt)))]
     (doseq [e (:Es result)]
       (is (cn/instance-of? :LEnt/E e))
       (is (some #{(:X e)} xs))))))
