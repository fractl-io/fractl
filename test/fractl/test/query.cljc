(ns fractl.test.query
  #?(:clj (:use [fractl.lang]))
  (:require [clojure.test :refer [deftest is]]
            [fractl.component :as cn]
            [fractl.eval :as e])
  #?(:cljs [fractl.lang
            :refer [component attribute event
                    entity record dataflow]]))

(deftest q01
  (component :Q01)
  (entity {:Q01/E {:X :Kernel/Int
                   :Y :Kernel/Int}})
  (event {:Q01/QE01 {:Y :Kernel/Int}})
  (dataflow :Q01/QE01
            {:Q01/E {:X? [:>= 10]
                     :Y :Q01/QE01.Y}})
  (event {:Q01/QE02 {:X :Kernel/Int
                     :Y :Kernel/Int}})
  (dataflow :Q01/QE02
            {:Q01/E {:X? [:>= :Q01/QE02.X]
                     :Y? :Q01/QE02.Y}})
  (let [es [(cn/make-instance :Q01/E {:X 10 :Y 4})
            (cn/make-instance :Q01/E {:X 12 :Y 6})
            (cn/make-instance :Q01/E {:X 9 :Y 3})]
        evts (map #(cn/make-instance :Q01/Create_E {:Instance %}) es)
        insts (map #(:result (first (e/eval-all-dataflows-for-event %))) evts)
        ids (map :Id insts)]
    (is (every? true? (map #(cn/instance-of? :Q01/E %) insts)))
    (let [evt01 (cn/make-instance :Q01/QE01 {:Y 100})
          r01 (e/eval-all-dataflows-for-event evt01)
          evt02 (cn/make-instance :Q01/QE02 {:X 10 :Y 100})
          r02 (e/eval-all-dataflows-for-event evt02)]
      ;; TODO: test eval results
      true)))
