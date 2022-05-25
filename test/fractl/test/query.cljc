(ns fractl.test.query
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            #?(:clj  [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest q01
  (defcomponent :Q01
    (entity {:Q01/E {:X :Kernel/Int}}))
  (let [e (cn/make-instance :Q01/E {:X 10})
        e1 (first (tu/fresult (e/eval-all-dataflows {:Q01/Upsert_E {:Instance e}})))
        id (:Id e1)
        e2 (first (tu/fresult (e/eval-all-dataflows {:Q01/Lookup_E {:Id id}})))]
    (is (cn/instance-of? :Q01/E e2))
    (is (cn/same-instance? e1 e2))))

(deftest q02
  (defcomponent :Q02
    (entity {:Q02/E {:X {:type :Kernel/Int
                         :indexed true}
                     :Y {:type :Kernel/Int
                         :indexed true}}})
    (event {:Q02/QE01 {:Y :Kernel/Int}})
    (dataflow :Q02/QE01
              {:Q02/E {:X? [:>= 10]
                       :Y :Q02/QE01.Y}})
    (event {:Q02/QE02 {:X :Kernel/Int
                       :Y :Kernel/Int}})
    (dataflow :Q02/QE02
              {:Q02/E {:X? [:>= :Q02/QE02.X]
                       :Y? :Q02/QE02.Y}}))
  (let [es [(cn/make-instance :Q02/E {:X 10 :Y 4})
            (cn/make-instance :Q02/E {:X 12 :Y 6})
            (cn/make-instance :Q02/E {:X 9 :Y 3})]
        evts (map #(cn/make-instance :Q02/Upsert_E {:Instance %}) es)
        f (comp first #(:result (first (e/eval-all-dataflows %))))
        insts (mapv f evts)
        ids (mapv :Id insts)]
    (is (every? true? (map #(cn/instance-of? :Q02/E %) insts)))
    (let [r01 (tu/fresult (e/eval-all-dataflows {:Q02/QE01 {:Y 100}}))
          r (e/eval-all-dataflows {:Q02/QE02 {:X 5 :Y 100}})
          r02 (tu/fresult r)
          fs01 (:from (:transition r01))
          ts01 (:to (:transition r01))]
      (is (= 2 (count r01)))
      (is (every? #(and (some #{(:X %)} [10 12]) (some #{(:Y %)} [4 6])) fs01))
      (is (every? #(and (some #{(:X %)} [10 12]) (= 100 (:Y %))) ts01))
      (is (= 2 (count r02)))
      (is (every? #(and (some #{(:X %)} [10 12]) (= 100 (:Y %))) r02)))))

(deftest query-all
  (defcomponent :QueryAll
    (entity {:QueryAll/E {:X :Kernel/Int :N :Kernel/String}})
    (event {:QueryAll/AllE {}})
    (dataflow :QueryAll/AllE
              :QueryAll/E?))
  (let [es [(cn/make-instance :QueryAll/E {:X 1 :N "e01"})
            (cn/make-instance :QueryAll/E {:X 2 :N "e02"})]
        evts (mapv #(cn/make-instance :QueryAll/Upsert_E {:Instance %}) es)
        _ (mapv tu/fresult (mapv #(e/eval-all-dataflows %) evts))
        result (tu/fresult (e/eval-all-dataflows {:QueryAll/AllE {}}))]
    (doseq [r result]
      (is (cn/instance-of? :QueryAll/E r))
      (is (= (if (= 1 (:X r)) "e01" "e02") (:N r))))))

(deftest alias-on-query-result
  (defcomponent :QueryAlias
    (entity {:QueryAlias/E {:X {:type :Kernel/Int
                                :indexed true}
                            :N :Kernel/String}})
    (event {:QueryAlias/Evt {:X :Kernel/Int}})
    (dataflow :QueryAlias/Evt
              {:QueryAlias/E {:X? :QueryAlias/Evt.X} :as :R}
              :R))
  (let [es [(cn/make-instance :QueryAlias/E {:X 1 :N "e01"})
            (cn/make-instance :QueryAlias/E {:X 2 :N "e02"})
            (cn/make-instance :QueryAlias/E {:X 1 :N "e03"})]
        evts (map #(cn/make-instance :QueryAlias/Upsert_E {:Instance %}) es)
        es_result (doall (map (comp (comp first tu/fresult)
                                    #(e/eval-all-dataflows %))
                              evts))
        result (tu/fresult (e/eval-all-dataflows {:QueryAlias/Evt {:X 1}}))]
    (doseq [r result]
      (is (cn/instance-of? :QueryAlias/E r))
      (is (= 1 (:X r)))
      (is (let [n (:N r)]
            (some #{n} #{"e01" "e03"}))))))

(deftest query-alias-in-expr
  (defcomponent :QueryAliasInExpr
    (entity {:QueryAliasInExpr/OrderLine
             {:Title :Kernel/String
              :Qty :Kernel/Int}})
    (entity {:QueryAliasInExpr/ProductBatch
             {:Title :Kernel/String
              :AvailableQty {:type :Kernel/Int :check pos?}}})
    (dataflow :QueryAliasInExpr/AllocateOrderLine
              {:QueryAliasInExpr/OrderLine
               {:Id? :QueryAliasInExpr/AllocateOrderLine.LineId}
               :as :OL}
              {:QueryAliasInExpr/ProductBatch
               {:Id? :QueryAliasInExpr/AllocateOrderLine.BatchId
                :AvailableQty '(- :AvailableQty :OL.Qty)}}))
  (let [batch (cn/make-instance
               {:QueryAliasInExpr/ProductBatch
                {:Title "Table"
                 :AvailableQty 20}})
        evt (cn/make-instance
             {:QueryAliasInExpr/Upsert_ProductBatch
              {:Instance batch}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))
        batch-id (:Id r)
        order-line (cn/make-instance
                    {:QueryAliasInExpr/OrderLine
                     {:Title "Table"
                      :Qty 21}})
           evt (cn/make-instance
                {:QueryAliasInExpr/Upsert_OrderLine
                 {:Instance order-line}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))
        line-id (:Id r)
        evt (cn/make-instance
             {:QueryAliasInExpr/AllocateOrderLine
              {:BatchId batch-id :LineId line-id}})
        _ (tu/is-error #(doall (e/eval-all-dataflows evt)))
        order-line (cn/make-instance
                    {:QueryAliasInExpr/OrderLine
                     {:Title "Table"
                      :Qty 2}})
        evt (cn/make-instance
             {:QueryAliasInExpr/Upsert_OrderLine
              {:Instance order-line}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))
        line-id (:Id r)
        evt (cn/make-instance
             {:QueryAliasInExpr/AllocateOrderLine
              {:BatchId batch-id :LineId line-id}})
        r (get-in
           (first (tu/fresult (e/eval-all-dataflows evt)))
           [:transition :to])]
    (is (= (:AvailableQty r) 18))))

(deftest idempotent-upsert
  (defcomponent :IdUps
    (entity {:IdUps/E {:X {:type :Kernel/Int
                           :unique true
                           :indexed true}
                       :Y :Kernel/Int}})
    (let [e1 (cn/make-instance :IdUps/E {:X 10 :Y 20})
          r1 (first (tu/fresult (e/eval-all-dataflows {:IdUps/Upsert_E {:Instance e1}})))
          id (:Id r1)
          r2 (first (tu/fresult (e/eval-all-dataflows {:IdUps/Lookup_E {:Id id}})))
          e2 (cn/make-instance :IdUps/E {:X 10 :Y 30})
          r3 (first (tu/fresult (e/eval-all-dataflows {:IdUps/Upsert_E {:Instance e2}})))
          r4 (first (tu/fresult (e/eval-all-dataflows {:IdUps/Lookup_E {:Id id}})))]
      (is (= 20 (get-in r3 [:transition :from :Y])))
      (is (= 30 (get-in r3 [:transition :to :Y])))
      (is (= id (:Id r4)))
      (is (= (:X r2) (:X r4)))
      (is (= 20 (:Y r2)))
      (is (= 30 (:Y r4))))))

(deftest query-by-id-and-delete
  (defcomponent :QIdDel
    (entity {:QIdDel/E {:X {:type :Kernel/Int
                            :indexed true}}})
    (event {:QIdDel/FindByIdAndDel
            {:EId :Kernel/UUID}})
    (dataflow :QIdDel/FindByIdAndDel
              [:delete :QIdDel/E :QIdDel/FindByIdAndDel.EId]))
  (let [e (cn/make-instance :QIdDel/E {:X 100})
        e01 (first (tu/fresult (e/eval-all-dataflows {:QIdDel/Upsert_E {:Instance e}})))
        id (:Id e01)
        devt (cn/make-instance :QIdDel/FindByIdAndDel {:EId (:Id e01)})
        _ (doall (e/eval-all-dataflows devt))
        levt (cn/make-instance :QIdDel/Lookup_E {:Id id})
        lookup-result (doall (e/eval-all-dataflows levt))]
    (is (= :not-found (:status (first lookup-result))))))

(deftest query-and-delete
  (defcomponent :QDel
    (entity {:QDel/E {:X {:type :Kernel/Int
                          :indexed true}}})
    (event {:QDel/FindAndDel
            {:X :Kernel/Int}})
    (dataflow :QDel/FindAndDel
              {:QDel/E {:X? :QDel/FindAndDel.X}}
              [:delete :QDel/E :QDel/E.Id])
    (dataflow :QDel/DeleteById
              [:delete :QDel/E :QDel/DeleteById.EId]))
  (let [e (cn/make-instance :QDel/E {:X 100})
        e01 (first (tu/fresult (e/eval-all-dataflows {:QDel/Upsert_E {:Instance e}})))
        id (:Id e01)
        devt (cn/make-instance :QDel/FindAndDel {:X 100})
        _ (doall (e/eval-all-dataflows devt))
        levt (cn/make-instance :QDel/Lookup_E {:Id id})
        lookup-result (doall (e/eval-all-dataflows levt))
        devt (cn/make-instance :QDel/FindAndDel {:X 100})
        del-result1 (doall (e/eval-all-dataflows devt))
        devt (cn/make-instance :QDel/DeleteById {:EId id})
        del-result2 (doall (e/eval-all-dataflows devt))]
    (is (= :not-found (:status (first del-result1))))
    (is (= :ok (:status (first del-result2))))
    (is (= :not-found (:status (first lookup-result))))))

(deftest issue-255-query-non-indexed
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I255
     (entity {:I255/E {:X {:type :Kernel/Int
                           :indexed true}
                       :Y :Kernel/Int}})
     (dataflow :I255/Q1
               {:I255/E {:X? :I255/Q1.X
                         :Y? [:< 5]}})
     (dataflow :I255/Q2
               {:I255/E {:X? :I255/Q2.X
                         :Y? [:or [:> :X] [:= :I255/Q2.Y]]}}))
   (let [es [(cn/make-instance :I255/E {:X 10 :Y 4})
             (cn/make-instance :I255/E {:X 10 :Y 6})
             (cn/make-instance :I255/E {:X 10 :Y 3})
             (cn/make-instance :I255/E {:X 9 :Y 2})
             (cn/make-instance :I255/E {:X 10 :Y 20})]
         evts (map #(cn/make-instance :I255/Upsert_E {:Instance %}) es)
         f (comp first #(:result (first (e/eval-all-dataflows %))))
         insts (map f evts)]
     (is (every? true? (map #(cn/instance-of? :I255/E %) insts)))
     (let [r (tu/fresult (e/eval-all-dataflows {:I255/Q1 {:X 10}}))]
       (is (= (count r) 2))
       (doseq [e r]
         (is (and (= 10 (:X e))
                  (< (:Y e) 5)))))
     (let [r (tu/fresult (e/eval-all-dataflows {:I255/Q2 {:X 10 :Y 3}}))]
       (is (= (count r) 1))
       (doseq [e r]
         (is (and (= 10 (:X e))
                  (or (> (:Y e) 10)
                      (= (:Y e) 3)))))))))

(deftest test-unique-date-time
  (defcomponent :Dt01
    (entity {:Dt01/E {:Name :Kernel/String
                      :LastAccountAccess
                      {:type :Kernel/DateTime
                       ;; Disable this for postgres
                                        ;:unique true
                       }}}))
  (let [e (cn/make-instance :Dt01/E {:Name "Birkhe" :LastAccountAccess "2018-07-28T12:15:30"})
        e1 (first (tu/fresult (e/eval-all-dataflows {:Dt01/Upsert_E {:Instance e}})))
        id (:Id e1)
        e2 (first (tu/fresult (e/eval-all-dataflows {:Dt01/Lookup_E {:Id id}})))
        laa (:LastAccountAccess e2)]
    (is (cn/instance-of? :Dt01/E e2))
    (is (cn/same-instance? e1 e2))
    (is (= "2018-07-28T12:15:30" laa))))

(deftest query-in-for-each
  (defcomponent :Qfe
    (entity {:Qfe/E {:X {:type :Kernel/Int
                         :indexed true}}})
    (record {:Qfe/R {:Y :Kernel/Int}})
    (dataflow
     :Qfe/Evt1
     [:for-each :Qfe/E? {:Qfe/R {:Y '(+ 10 :Qfe/E.X)}}])
    (dataflow
     :Qfe/Evt2
     [:for-each {:Qfe/E {:X? 20}}
      {:Qfe/R {:Y '(+ 10 :Qfe/E.X)}}])
    (dataflow
     :Qfe/Evt3
     {:Qfe/E {:X :Qfe/Evt3.I} :as :E1}
     {:Qfe/E {:X '(+ :E1.X 1)} :as :E2}
     [:for-each {:Qfe/E {:X? 10}}
      {:Qfe/R {:Y :Qfe/E.X}}])
    (dataflow
     :Qfe/Evt4
     {:Qfe/E {:X :Qfe/Evt4.I} :as :E1}
     {:Qfe/E {:X '(+ :E1.X 1)} :as :E2}
     [:for-each :Qfe/E?
      {:Qfe/R {:Y :Qfe/E.X}}]))
  (defn make-e [x]
    (let [evt (cn/make-instance
               {:Qfe/Upsert_E
                {:Instance
                 (cn/make-instance {:Qfe/E {:X x}})}})
          e (first (tu/fresult (e/eval-all-dataflows evt)))]
      (is (cn/instance-of? :Qfe/E e))
      (is (= x (:X e)))
      e))
  (let [e1 (make-e 10)
        e2 (make-e 20)
        evt1 (cn/make-instance {:Qfe/Evt1 {}})
        rs1 (tu/fresult (e/eval-all-dataflows evt1))
        evt2 (cn/make-instance {:Qfe/Evt2 {:X 20}})
        rs2 (tu/fresult (e/eval-all-dataflows evt2))
        evt3 (cn/make-instance {:Qfe/Evt3 {:I 10}})
        rs3 (tu/fresult (e/eval-all-dataflows evt3))
        evt4 (cn/make-instance {:Qfe/Evt4 {:I 100}})
        rs4 (tu/fresult (e/eval-all-dataflows evt4))]
    (doseq [r rs1]
      (is (cn/instance-of? :Qfe/R r))
      (let [y (:Y r)]
        (is (or (= y 20) (= y 30)))))
    (is (= 1 (count rs2)))
    (is (= 30 (:Y (first rs2))))
    (is (= 2 (count rs3)))
    (doseq [r rs3]
      (is (= 10 (:Y r))))
    (is (= 6 (count rs4)))
    (doseq [r rs4]
      (let [y (:Y r)]
        (some #{y} #{10 11 20 100 101})))))

(deftest like-operator
  (defcomponent :LikeOpr
    (entity
     {:LikeOpr/E
      {:X {:type :Kernel/String
           :indexed true}}})
    (dataflow
     :LikeOpr/Q
     {:LikeOpr/E
      {:X? [:like :LikeOpr/Q.S]}}))
  (let [e1 (cn/make-instance :LikeOpr/E {:X "hi"})
        e2 (cn/make-instance :LikeOpr/E {:X "bye"})
        e3 (cn/make-instance :LikeOpr/E {:X "hello"})
        [r1 r2 r3] (mapv #(tu/first-result {:LikeOpr/Upsert_E {:Instance %}}) [e1 e2 e3])
        qrs1 (tu/fresult (e/eval-all-dataflows {:LikeOpr/Q {:S "h%"}}))
        qrs2 (tu/fresult (e/eval-all-dataflows {:LikeOpr/Q {:S "b%"}}))
        qrs3 (tu/fresult (e/eval-all-dataflows {:LikeOpr/Q {:S "%ell%"}}))]
    (doseq [r qrs1]
      (is (or (cn/same-instance? r1 r) (cn/same-instance? r3 r))))
    (is (cn/same-instance? (first qrs2) r2))
    (is (cn/same-instance? (first qrs3) r3))))

(deftest query-command
  (defcomponent :QueryCommand
    (entity
     :QueryCommand/E
     {:X {:type :Kernel/Int
          :indexed true}
      :Y :Kernel/Int})
    (record
     :QueryCommand/F
     {:A :Kernel/Int
      :B :Kernel/Int})
    (dataflow
     :QueryCommand/FindE
     [:query :QueryCommand/FindE.Q])
    (dataflow
     :QueryCommand/EtoF
     [:query :QueryCommand/EtoF.Q :as :R]
     [:for-each
      :R
      {:QueryCommand/F
       {:A :%.X
        :B :%.Y}}]))
  (let [es (mapv
            #(tu/first-result
              (cn/make-instance
               {:QueryCommand/Upsert_E
                {:Instance
                 (cn/make-instance
                  {:QueryCommand/E
                   {:X %1 :Y %2}})}}))
            [10 20 30 40]
            [9 7 12 1])
        rs01 (:result
              (first
               (e/eval-all-dataflows
                (cn/make-instance
                 {:QueryCommand/FindE
                  {:Q {:QueryCommand/E?
                       {:where [:>= :X 20]
                        :order-by [:Y]}}}}))))
        rs02 (:result
              (first
               (e/eval-all-dataflows
                (cn/make-instance
                 {:QueryCommand/EtoF
                  {:Q {:QueryCommand/E?
                       {:where [:>= :X 20]
                        :order-by [:Y]}}}}))))]
    (is (every? #(>= (:X %) 20) rs01))
    (is (apply < (mapv :Y rs01)))
    (is (every? (partial cn/instance-of? :QueryCommand/F) rs02))
    (is (every? #(>= (:A %) 20) rs02))
    (is (apply < (mapv :B rs02)))))

(deftest ref-first-result
  (defcomponent :Rfr
    (record {:Rfr/R {:A :Kernel/Int :B :Kernel/Int}})
    (entity {:Rfr/E {:N {:type :Kernel/String :indexed true} :X :Kernel/Int}})
    (dataflow
     :Rfr/J
     {:Rfr/E {:N? :Rfr/J.N1} :as :E1}
     {:Rfr/E {:N? :Rfr/J.N2} :as :E2}
     {:Rfr/R {:A :E1.X :B :E2.X}}))
  (let [e1 (cn/make-instance :Rfr/E {:N "ABC" :X 10})
        e2 (cn/make-instance :Rfr/E {:N "EFG" :X 20})
        r1 (first (tu/fresult (e/eval-all-dataflows {:Rfr/Upsert_E {:Instance e1}})))
        r2 (first (tu/fresult (e/eval-all-dataflows {:Rfr/Upsert_E {:Instance e2}})))
        k1 (first (tu/fresult (e/eval-all-dataflows {:Rfr/J {:N1 "EFG" :N2 "ABC"}})))]
    (is (cn/instance-of? :Rfr/R k1))
    (is (= 20 (:A k1)))
    (is (= 10 (:B k1)))))
