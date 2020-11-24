(ns fractl.test.resolver
  #?(:clj (:use [fractl.lang]))
  (:require [clojure.test :refer [deftest is]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])
            [fractl.component :as cn]
            [fractl.resolver :as r])
  #?(:cljs [fractl.lang
            :refer [component attribute event
                    entity record dataflow]]))

(def eval-all-dataflows-for-event (tu/make-df-eval))

(defn- compose-test-resolver [path]
  (let [r (r/make-resolver :TestResolver
                           {:upsert identity})]
    (r/compose-resolver path r)))

(deftest r01
  (defcomponent :R01
    (entity {:R01/E {:X :Kernel/Int}}))
  (compose-test-resolver :R01/E)
  (let [e (cn/make-instance :R01/E {:X 10})
        evt (cn/make-instance :R01/Create_E {:Instance e})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        e01 (ffirst result)
        r (ffirst (second result))]
    (is (cn/instance-of? :R01/E e01))
    (is (= :TestResolver (:resolver r)))
    (is (= :upsert (:method r)))
    (is (= e01 (:result r)))))
