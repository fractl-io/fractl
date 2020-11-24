(ns fractl.test.resolver
  #?(:clj (:use [fractl.lang]))
  (:require [clojure.test :refer [deftest is]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver :as r])
  #?(:cljs [fractl.lang
            :refer [component attribute event
                    entity record dataflow]]))

(def eval-all-dataflows-for-event (tu/make-df-eval))

(defn- compose-test-resolver [path state]
  (let [r (r/make-resolver :TestResolver
                           {:upsert #(u/safe-set state [:upsert %1])})]
    (r/compose-resolver path r)))

(deftest r01
  (defcomponent :R01
    (entity {:R01/E {:X :Kernel/Int}}))
  (compose-test-resolver :R01/E (u/make-cell))
  (let [e (cn/make-instance :R01/E {:X 10})
        evt (cn/make-instance :R01/Create_E {:Instance e})
        e1 (tu/fresult (eval-all-dataflows-for-event evt))]
    ))
