(ns fractl.test.reagent
  (:require [cljs.test :refer-macros [deftest is]]
            [fractl.lang
             :refer [event entity dataflow]]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.store :as store]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.test.util :as tu :refer-macros [defcomponent]]))

(def store (store/open-reagent-store nil))

(def eval-all-dataflows-for-event (e/evaluator store nil))

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

(deftest store-test
  (defcomponent :ST
    (entity {:ST/E {:X :Kernel/Int}})

    (event {:ST/New_E {:X :Kernel/Int}})

    (dataflow :ST/New_E
              {:ST/E {:X :ST/New_E.X}}
              :ST/E.X))

  (let [e (cn/make-instance :ST/E {:X 10})
        evt (cn/make-instance :ST/Upsert_E {:Instance e})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        e01 (ffirst result)]
    (is (cn/instance-of? :ST/E e01))
    (is (nil? (second result)))
    (is (persisted? :ST e01)))
  (compose-test-resolver :TestResolver01 :ST/E)
  (let [e (cn/make-instance :ST/E {:X 10})
        evt (cn/make-instance :ST/Upsert_E {:Instance e})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        e01 (ffirst result)
        r (ffirst (second result))]
    (is (cn/instance-of? :ST/E e01))
    (is (persisted? :ST e01))
    (is (cn/instance-of? :ST/E r))
    (is (= e01 r))
    (let [evt (cn/make-instance :ST/New_E {:X 100})
          result (tu/fresult (eval-all-dataflows-for-event evt))]
      (is (fn? result))
      (is (= (result) 100)))))
