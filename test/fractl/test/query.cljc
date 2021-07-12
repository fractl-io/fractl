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
        e1 (ffirst (tu/fresult (e/eval-all-dataflows {:Q01/Upsert_E {:Instance e}})))
        id (:Id e1)
        e2 (ffirst (tu/fresult (e/eval-all-dataflows {:Q01/Lookup_E {:Id id}})))]
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
        f (comp ffirst #(:result (first (e/eval-all-dataflows %))))
        insts (map f evts)
        ids (map :Id insts)]
    (is (every? true? (map #(cn/instance-of? :Q02/E %) insts)))
    (let [r01 (first (tu/fresult (e/eval-all-dataflows {:Q02/QE01 {:Y 100}})))
          r (e/eval-all-dataflows {:Q02/QE02 {:X 10 :Y 100}})
          r02 (first (tu/fresult r))]
      (is (= 2 (count r01)))
      (is (every? #(and (>= (:X %) 10) (= (:Y %) 100)) r01))
      (is (= 2 (count r02)))
      (is (every? #(and (>= (:X %) 10) (= (:Y %) 100)) r02)))))

(deftest query-all
  (defcomponent :QueryAll
    (entity {:QueryAll/E {:X :Kernel/Int :N :Kernel/String}})
    (event {:QueryAll/AllE {}})
    (dataflow :QueryAll/AllE
              :QueryAll/E?))
  (let [es [(cn/make-instance :QueryAll/E {:X 1 :N "e01"})
            (cn/make-instance :QueryAll/E {:X 2 :N "e02"})]
        evts (map #(cn/make-instance :QueryAll/Upsert_E {:Instance %}) es)
        _ (doall (map tu/fresult (map #(e/eval-all-dataflows %) evts)))
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
        es_result (doall (map (comp (comp ffirst tu/fresult)
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
        r (ffirst (tu/fresult (e/eval-all-dataflows evt)))
        batch-id (:Id r)
        order-line (cn/make-instance
                    {:QueryAliasInExpr/OrderLine
                     {:Title "Table"
                      :Qty 21}})
        evt (cn/make-instance
             {:QueryAliasInExpr/Upsert_OrderLine
              {:Instance order-line}})
        r (ffirst (tu/fresult (e/eval-all-dataflows evt)))
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
        r (ffirst (tu/fresult (e/eval-all-dataflows evt)))
        line-id (:Id r)
        evt (cn/make-instance
             {:QueryAliasInExpr/AllocateOrderLine
              {:BatchId batch-id :LineId line-id}})
        r (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (is (= (:AvailableQty r) 18))))

(deftest idempotent-upsert
  (defcomponent :IdUps
    (entity {:IdUps/E {:X {:type :Kernel/Int
                           :unique true
                           :indexed true}
                       :Y :Kernel/Int}})
    (let [e1 (cn/make-instance :IdUps/E {:X 10 :Y 20})
          r1 (ffirst (tu/fresult (e/eval-all-dataflows {:IdUps/Upsert_E {:Instance e1}})))
          id (:Id r1)
          r2 (ffirst (tu/fresult (e/eval-all-dataflows {:IdUps/Lookup_E {:Id id}})))
          e2 (cn/make-instance :IdUps/E {:X 10 :Y 30})
          r3 (ffirst (tu/fresult (e/eval-all-dataflows {:IdUps/Upsert_E {:Instance e2}})))
          r4 (ffirst (tu/fresult (e/eval-all-dataflows {:IdUps/Lookup_E {:Id id}})))]
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
        e01 (ffirst (tu/fresult (e/eval-all-dataflows {:QIdDel/Upsert_E {:Instance e}})))
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
        e01 (ffirst (tu/fresult (e/eval-all-dataflows {:QDel/Upsert_E {:Instance e}})))
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
                         :Y? [:< :Y 5]}})
     (dataflow :I255/Q2
               {:I255/E {:X? :I255/Q2.X
                         :Y? [:or [:> :Y :X] [:= :Y :I255/Q2.Y]]}}))
   (let [es [(cn/make-instance :I255/E {:X 10 :Y 4})
             (cn/make-instance :I255/E {:X 10 :Y 6})
             (cn/make-instance :I255/E {:X 10 :Y 3})
             (cn/make-instance :I255/E {:X 9 :Y 2})
             (cn/make-instance :I255/E {:X 10 :Y 20})]
         evts (map #(cn/make-instance :I255/Upsert_E {:Instance %}) es)
         f (comp ffirst #(:result (first (e/eval-all-dataflows %))))
         insts (map f evts)]
     (is (every? true? (map #(cn/instance-of? :I255/E %) insts)))
     (let [r (first (tu/fresult (e/eval-all-dataflows {:I255/Q1 {:X 10}})))]
       (is (= (count r) 2))
       (doseq [e r]
         (is (and (= 10 (:X e))
                  (< (:Y e) 5)))))
     (let [r (first (tu/fresult (e/eval-all-dataflows {:I255/Q2 {:X 10 :Y 3}})))]
       (is (= (count r) 2))
       (doseq [e r]
         (is (and (= 10 (:X e))
                  (or (> (:Y e) 10)
                      (= (:Y e) 3)))))))))

(deftest test-unique-date-time
  (defcomponent :Dt01
                (entity {:Dt01/E {:Name :Kernel/String
                                  :LastAccountAccess {:type :Kernel/DateTime
                                                      ;; Disable this for postgres
                                                      ;:unique true
                                                      }}}))
  (let [e (cn/make-instance :Dt01/E {:Name "Birkhe" :LastAccountAccess "2018-07-28T12:15:30"})
        e1 (ffirst (tu/fresult (e/eval-all-dataflows {:Dt01/Upsert_E {:Instance e}})))
        id (:Id e1)
        e2 (ffirst (tu/fresult (e/eval-all-dataflows {:Dt01/Lookup_E {:Id id}})))
        laa (:LastAccountAccess e2)]
    (is (cn/instance-of? :Dt01/E e2))
    (is (cn/same-instance? e1 e2))
    (is (= "2018-07-28T12:15:30" laa))))
