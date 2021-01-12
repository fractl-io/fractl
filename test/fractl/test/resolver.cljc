(ns fractl.test.resolver
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(def eval-all-dataflows-for-event (tu/make-df-eval))

(defn- test-resolver [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name {:upsert identity
                                          :delete (fn [x] x)})]
    (install-resolver path r)))

(def compose-test-resolver (partial test-resolver rg/compose-resolver))
(def override-test-resolver (partial test-resolver rg/override-resolver))

(defn- persisted? [comp-name entity-instance]
  (let [id (:Id entity-instance)
        evt (cn/make-instance (keyword (str (name comp-name) "/Lookup_E")) {:Id id})
        result (eval-all-dataflows-for-event evt)
        r (first result)]
    (when-not (= :not-found (:status r))
      (let [e (ffirst (:result r))]
        (cn/same-instance? entity-instance e)))))

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
    (is (cn/instance-of? :R01/E r))
    (is (= e01 r))
    (let [id (:Id e01)
          evt (cn/make-instance :R01/Delete_E {:Id id})
          result (tu/fresult (eval-all-dataflows-for-event evt))
          r01 (first result)
          r02 (ffirst (second result))]
      (is (= r01 [[:R01 :E] id]))
      (is (= r02 [[:R01 :E] id])))))

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
    (is (= e01 r))))

(defn- test-query-resolver [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name {:query (fn [arg]
                                                   (let [id (nth (:where (:raw-query arg)) 2)]
                                                     [(cn/make-instance :RQ/E {:X 1 :Id id})]))})]
    (install-resolver path r)))

(deftest query
  (defcomponent :RQ
    (entity {:RQ/E {:X :Kernel/Int}}))
  (test-query-resolver rg/compose-resolver :RQResolver :RQ/E)
  (let [e (cn/make-instance :RQ/E {:X 10})
        evt (cn/make-instance :RQ/Upsert_E {:Instance e})
        e01 (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :RQ/E e01))
    (is (= 10 (:X e01)))
    (let [id (:Id e01)
          evt (cn/make-instance :RQ/Lookup_E {:Id id})
          e02 (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
      (is (cn/instance-of? :RQ/E e02))
      ;(is (= id (:Id e02)))
      (is (= 1 (:X e02))))))
