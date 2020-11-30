(ns fractl.test.resolver
  #?(:clj (:use [fractl.lang]))
  (:require [clojure.test :refer [deftest is]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])
            [fractl.component :as cn]
            [fractl.resolver.core :as r])
  #?(:cljs [fractl.lang
            :refer [component attribute event
                    entity record dataflow]]))

(def eval-all-dataflows-for-event (tu/make-df-eval))

(defn- test-resolver [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name {:upsert identity})]
    (install-resolver path r)))

(def compose-test-resolver (partial test-resolver r/compose-resolver))
(def override-test-resolver (partial test-resolver r/override-resolver))

(defn- persisted? [comp-name entity-instance]
  (let [id (:Id entity-instance)
        evt (cn/make-instance (keyword (str (name comp-name) "/Lookup_E")) {:Id id})
        e (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (cn/same-instance? entity-instance e)))

(deftest r01
  (defcomponent :R01
    (entity {:R01/E {:X :Kernel/Int}}))
  (let [e (cn/make-instance :R01/E {:X 10})
        evt (cn/make-instance :R01/Upsert_E {:Instance e})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        e01 (ffirst result)]
    (is (cn/instance-of? :R01/E e01))
    (is (nil? (second result)))
    (is (persisted? :R01 e01)))
  (compose-test-resolver :TestResolver01 :R01/E)
  (let [e (cn/make-instance :R01/E {:X 10})
        evt (cn/make-instance :R01/Upsert_E {:Instance e})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        e01 (ffirst result)
        r (ffirst (second result))]
    (is (cn/instance-of? :R01/E e01))
    (is (persisted? :R01 e01))
    (is (= :TestResolver01 (:resolver r)))
    (is (= :upsert (:method r)))
    (is (= e01 (:result r)))))

(deftest r02
  (defcomponent :R02
    (entity {:R02/E {:X :Kernel/Int}}))
  (override-test-resolver :TestResolver02 :R02/E)
  (let [e (cn/make-instance :R02/E {:X 10})
        evt (cn/make-instance :R02/Upsert_E {:Instance e})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        e01 (ffirst result)
        r (ffirst (second result))]
    (is (cn/instance-of? :R02/E e01))
    (is (not (persisted? :R02 e01)))
    (is (= :TestResolver02 (:resolver r)))
    (is (= :upsert (:method r)))
    (is (= e01 (:result r)))))
