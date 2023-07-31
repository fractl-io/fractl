(ns fractl.test.fixes01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow relationship]]
            [fractl.evaluator :as e]
            [fractl.lang.datetime :as dt]
            [fractl.lang.raw :as raw]
            [fractl.compiler.rule :as rule]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-195
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I195
     (entity {:I195/E1 {:A :Int
                        :B :Int
                        :C :Int
                        :Y :DateTime}})
     (dataflow :I195/K
               {:I195/E1 {:A '(+ 5 :B)
                          :B 10
                          :C '(+ 10 :A)
                          :Y '(fractl.lang.datetime/now)}})
     (entity {:I195/E2 {:Y :DateTime}})
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

(defn- assert-transition [attr-names to-attr-vals r]
  (is (= to-attr-vals (mapv #(% r) attr-names))))

(deftest issue-196
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I196
     (entity
      :I196/E1
      {:Id {:type :Int :identity true}
       :A :Int
       :B :Int
       :C :Int
       :meta {:unique [:A :C]}}))
   (let [e01 (cn/make-instance :I196/E1 {:Id 1 :A 10 :B 20 :C 30})
         evt1 (cn/make-instance {:I196/Create_E1 {:Instance e01}})
         e02 {:A 10 :B 40 :C 30}
         evt2 (cn/make-instance {:I196/Update_E1 {:Id 1 :Data e02}})
         e03 (cn/make-instance :I196/E1 {:Id 2 :A 20 :B 60 :C 40})
         evt3 (cn/make-instance {:I196/Create_E1 {:Instance e03}})
         e04 {:A 20 :B 40 :C 40}
         evt4 (cn/make-instance {:I196/Update_E1 {:Id 2 :Data e04}})
         results (mapv #(first (tu/fresult (e/eval-all-dataflows %)))
                       [evt1 evt2 evt3 evt4])]
     (is (cn/instance-of? :I196/E1 (first results)))
     (is (cn/instance-of? :I196/E1 (nth results 2)))
     (let [a (partial assert-transition [:A :B :C])]
       (a [10 40 30] (second results))
       (a [20 40 40] (nth results 3))))))

(deftest issue-206
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I206
     (entity
      :I206/E1
      {:Id {:type :Int :identity true}
       :A :Int
       :B :Int
       :C :Int
       :meta {:unique [:A :C]}}))
   (let [e01 (cn/make-instance :I206/E1 {:A 10 :B 20 :C 30 :Id 1})
         evt1 (cn/make-instance {:I206/Create_E1 {:Instance e01}})
         e02 {:A 10 :B 0 :C 30}
         evt2 (cn/make-instance {:I206/Update_E1 {:Id 1 :Data e02}})
         e03 {:A 10 :B 60 :C 30}
         evt3 (cn/make-instance {:I206/Update_E1 {:Id 1 :Data e03}})
         results (mapv #(first (tu/fresult (e/eval-all-dataflows %)))
                       [evt1 evt2 evt3])]
     (is (cn/instance-of? :I206/E1 (first results)))
     (let [a (partial assert-transition [:A :B :C])]
       (a [10 0 30] (second results))
       (a [10 60 30] (nth results 2))))))

(deftest issue-185
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I185
     (entity {:I185/E {:X :Int :Y :Int}})
     (record {:I185/R {:Y :Int}})
     (dataflow :I185/UpdateE
               {:I185/E {tu/q-id-attr (tu/append-id :I185/UpdateE)
                         :X :I185/UpdateE.X
                         :Y :I185/UpdateE.Y}})
     (dataflow [:I185/OnXGt10 :when [:and
                                     [:> :I185/E.X 10]
                                     [:= :I185/E.Y 200]]]
               {:I185/R {:Y '(* 2 :I185/E.Y)}}))
   (let [e (cn/make-instance {:I185/E {:X 10 :Y 1}})
         evt (cn/make-instance {:I185/Create_E {:Instance e}})
         r (tu/fresult (e/eval-all-dataflows evt))
         r1 (first r)
         id (cn/id-attr r1)
         evt (cn/make-instance {:I185/UpdateE {cn/id-attr id :X 20 :Y 100}})
         r2 (tu/fresult (e/eval-all-dataflows evt))
         r3 (first (tu/embedded-results r2))
         evt (cn/make-instance {:I185/UpdateE {cn/id-attr id :X 11 :Y 200}})
         r4 (tu/fresult (e/eval-all-dataflows evt))
         r5 (first (tu/embedded-results r4))
         evt (cn/make-instance {:I185/Lookup_E {cn/id-attr id}})
         r6 (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (nil? (tu/embedded-results r)))
     (is (cn/instance-of? :I185/E r1))
     (is (= 10 (:X r1)))
     (is (= 1 (:Y r1)))
     (let [inst (first r2)]
       (is (cn/instance-of? :I185/E inst))
       (is (= 20 (:X inst))))
     (is (nil? r3))
     (let [inst (first r4)]
       (is (cn/instance-of? :I185/E inst))
       (is (= 11 (:X inst)))
       (is (= 200 (:Y inst))))
     (is (cn/instance-of? :I185/R r5))
     (is (= 400 (:Y r5)))
     (is (cn/instance-of? :I185/E r6))
     (is (= 11 (:X r6)))
     (is (= 200 (:Y r6))))))

(deftest issue-213
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I213
     (entity {:I213/E1 {:X :Int}})
     (entity {:I213/E2 {:E1 {:ref (tu/append-id :I213/E1)}
                        :Y :Int}})
     (record {:I213/R {:Y :Int :Z :Int}})
     (dataflow :I213/UpdateE1
               {:I213/E1 {tu/q-id-attr (tu/append-id :I213/UpdateE1)
                          :X :I213/UpdateE1.X}})
     (dataflow :I213/UpdateE2
               {:I213/E2 {tu/q-id-attr (tu/append-id :I213/UpdateE2)
                          :Y :I213/UpdateE2.Y}})
     (dataflow [:I213/CrossCond
                :when [:and
                       [:> :I213/E1.X 10]
                       [:= :I213/E2.Y 200]]
                :on :I213/E2
                :where [:= :I213/E2.E1 (tu/append-id :I213/E1)]]
               {:I213/R {:Y '(* :I213/E2.Y :I213/E1.X) :Z 1}})
     (let [e1 (cn/make-instance {:I213/E1 {:X 10}})
           evt (cn/make-instance {:I213/Create_E1 {:Instance e1}})
           r1 (tu/fresult (e/eval-all-dataflows evt))
           e1 (first r1)
           e2 (cn/make-instance {:I213/E2 {:E1 (cn/id-attr e1)
                                           :Y 20}})
           evt (cn/make-instance {:I213/Create_E2 {:Instance e2}})
           r2 (tu/fresult (e/eval-all-dataflows evt))
           e2 (first r2)
           evt (cn/make-instance {:I213/UpdateE1
                                  {cn/id-attr (cn/id-attr e1)
                                   :X 20}})
           r3 (tu/fresult (e/eval-all-dataflows evt))
           e3 (first r3)
           evt (cn/make-instance {:I213/UpdateE2
                                  {cn/id-attr (cn/id-attr e2)
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
     (entity {:I213NR/E1 {:X :Int
                          :Z {:type :Int
                              :indexed true}}})
     (entity {:I213NR/E2 {:Y :Int}})
     (record {:I213NR/R {:Y :Int}})
     (dataflow [:I213NR/CrossCond
                :when [:and
                       [:> :I213NR/E1.X 10]
                       [:= :I213NR/E2.Y 200]]
                :on :I213NR/E2
                :where [:= :I213NR/E1.Z 1]]
               {:I213NR/R {:Y '(+ :I213NR/E1.X :I213NR/E2.Y)}})
     (let [e1 (cn/make-instance {:I213NR/E1 {:X 9 :Z 2}})
           evt (cn/make-instance {:I213NR/Create_E1 {:Instance e1}})
           r1 (tu/fresult (e/eval-all-dataflows evt))
           e1 (first r1)
           e2 (cn/make-instance {:I213NR/E2 {:Y 20}})
           evt (cn/make-instance {:I213NR/Create_E2 {:Instance e2}})
           r2 (tu/fresult (e/eval-all-dataflows evt))
           e2 (first r2)
           e11 (cn/make-instance {:I213NR/E1 {:X 11 :Z 1}})
           evt (cn/make-instance {:I213NR/Create_E1 {:Instance e11}})
           r11 (tu/fresult (e/eval-all-dataflows evt))
           e11 (first r11)
           e22 (cn/make-instance {:I213NR/E2 {:Y 200}})
           evt (cn/make-instance {:I213NR/Create_E2 {:Instance e22}})
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
     (event :I219/Evt {:Y :Int})
     (record :I219/R {:Y :Int
                      :Z :Map})
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
     (entity :lcase/e {:x :Int})
     (let [e (cn/make-instance
              {:lcase/e {:x 100}})]
       (is (cn/instance-of? :lcase/e e))
       (is (= 100 (:x e)))))))

(deftest issue-314-compound-exprs-in-records
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I314
     (record :I314/R {:X :Int
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
     (record {:LEnt/E {:X :Int}})
     (entity {:LEnt/F {:Es {:listof :LEnt/E}}})
     (event {:LEnt/MakeF1 {:Xs {:listof :Int}}})
     (event {:LEnt/MakeF2 {:Xs {:listof :Int}}})
     (dataflow
      :LEnt/MakeF1
      [:for-each :LEnt/MakeF1.Xs
       {:LEnt/E {:X :%}}
       :as :ListofEs]
      {:LEnt/F {:Es :ListofEs}})
     (dataflow
      :LEnt/MakeF2
      [:for-each [:LEnt/MakeF2.Xs :as :I]
       {:LEnt/E {:X '(* 10 :I)}}
       :as :ListofEs]
      {:LEnt/F {:Es :ListofEs}}))
   (let [xs [10 20 30 40]
         xs*10 (mapv #(* 10 %) xs)
         evt1 {:LEnt/MakeF1 {:Xs xs}}
         evt2 {:LEnt/MakeF2 {:Xs xs}}
         result1 (e/eval-all-dataflows evt1)
         result2 (e/eval-all-dataflows evt2)
         rs1 (first (tu/fresult result1))
         rs2 (first (tu/fresult result2))]
     (doseq [e (:Es rs1)]
       (is (cn/instance-of? :LEnt/E e))
       (is (some #{(:X e)} xs)))
     (doseq [e (:Es rs2)]
       (is (cn/instance-of? :LEnt/E e))
       (is (some #{(:X e)} xs*10))))))

(deftest for-each-alias
  (defcomponent :Fea
                (entity
                  :Fea/E
                  {:X :Int :Y :Int})
                (dataflow
                  :Fea/Evt1
                  [:for-each
                   :Fea/Evt1.Ys
                   {:Fea/E
                    {:X 10 :Y :%}}])
                (dataflow
                  :Fea/Evt2
                  [:for-each
                   :Fea/Evt2.Es
                   {:Fea/E
                    {:X :%.X :Y :%.Y}}]))
  (let [result1
        (tu/fresult
          (e/eval-all-dataflows
            {:Fea/Evt1
             {:Ys [20 3]}}))
        result2
        (tu/fresult
          (e/eval-all-dataflows
            {:Fea/Evt2
             {:Es [{:X 1 :Y 3}
                   {:X 2 :Y 4}]}}))]
    (doseq [e result1]
      (is (cn/instance-of? :Fea/E e))
      (is (= 10 (:X e)))
      (is (some #{(:Y e)} [20 3])))
    (doseq [e result2]
      (is (cn/instance-of? :Fea/E e))
      (is (some #{(:X e)} [1 2]))
      (is (some #{(:Y e)} [3 4])))))

(deftest issue-959
  (defcomponent :I959
    (entity
     :I959/A
     {:Name :String})

    (entity
     :I959/B
     {:Id {:type :UUID
           :identity true
           :default u/uuid-string}
      :Name :String})

    (relationship
     :I959/R
     {:meta {:contains [:I959/A :I959/B]}})

    (dataflow
     :I959/CreateB
     {:I959/A {:Name? "ABC"} :as [:A]}
     {:I959/B {:Name "A B"} :-> :A}))
  (let [a1 (tu/first-result {:I959/Create_A {:Instance {:I959/A {:Name "ABC"}}}})
        b1 (tu/first-result {:I959/CreateB {}})]
    (is (cn/instance-of? :I959/A a1))
    (is (cn/instance-of? :I959/B b1))))

(deftest issue-968-raw-delete
  (defcomponent :I968
    (attribute
     :I968/K
     {:type :Int :indexed true})
    (record
     :I968/A
     {:X :Int :Y :I968/K})
    (entity
     :I968/B
     {:Name :String})
    (event
     :I968/C
     {:X :Int :Y :Int})
    (dataflow
     :I968/C
     {:I968/A
      {:X :I968/C.X :Y :I968/C.Y}}))
  (defn a-def? [df x]
    (= (take 2 x) df))
  (defn any-def? [r df]
    (some (partial a-def? df) r))
  (let [dfs ['(attribute :I968/K) '(record :I968/A)
             '(entity :I968/B) '(event :I968/C) '(dataflow :I968/C)]
        p #(partial any-def? (rest (rest (raw/as-edn :I968))))]
    (is (every? (p) dfs))
    (mapv cn/remove-record [:I968/A :I968/B])
    (defn rem-dfs [names dfs]
      (loop [names names, result dfs]
        (if-let [n (first names)]
          (recur (rest names) (remove #(= n (second %)) result))
          result)))
    (is (every? (p) (rem-dfs [:I968/A :I968/B] dfs)))
    (mapv cn/remove-record [:I968/C :I968/K])
    (let [r (rest (raw/as-edn :I968))]
      (is (= '(component :I968) (first r)))
      (is (nil? (seq (rest r)))))
    (cn/remove-component :I968)
    (is (nil? (raw/as-edn :I968)))))
