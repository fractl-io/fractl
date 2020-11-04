(ns fractl.test.query
  #?(:clj (:use [fractl.lang]))
  (:require [clojure.test :refer [deftest is]]
            [fractl.component :as cn]
            [fractl.resolver :as r])
  #?(:cljs [fractl.lang
            :refer [component attribute event
                    entity record dataflow]]))

(deftest q01
  (component :Q01)
  (entity {:Q01/E {:X :Kernel/Int
                   :Y :Kernel/Int}})
  (let [es [(cn/make-instance :Q01/E {:X 10 :Y 4})
            (cn/make-instance :Q01/E {:X 12 :Y 6})
            (cn/make-instance :Q01/E {:X 9 :Y 3})]
        evts (map #(cn/make-instance :Q01/Create_E {:Instance %}) es)
        insts (map #(:result (first (r/eval-all-dataflows-for-event %))) evts)
        ids (map :Id insts)]
    (is (every? true? (map #(cn/instance-of? :Q01/E %) insts)))))
