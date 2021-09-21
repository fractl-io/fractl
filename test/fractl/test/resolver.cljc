(ns fractl.test.resolver
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.component :as cn]
            [fractl.store :as store]
            [fractl.evaluator :as e]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

#?(:clj
   (def store (store/open-default-store nil))
   :cljs
   (def store (store/open-default-store {:type :alasql})))

(defn- test-resolver [install-resolver resolver-name path]
  (let [f (fn [_ arg] arg)
        r (r/make-resolver resolver-name {:upsert {:handler identity
                                                   :xform {:in [f :EntityXformR01/EToEPrime]
                                                           :out [f :EntityXformR01/EPrimeToE]}}
                                          :delete {:handler identity
                                                   :xform {:in [f]}}}
                           e/eval-pure-dataflows)]
    (install-resolver path r)))

(def compose-test-resolver (partial test-resolver rg/compose-resolver))
(def override-test-resolver (partial test-resolver rg/override-resolver))

(defn- persisted? [comp-name entity-instance]
  (let [id (:Id entity-instance)
        evt (cn/make-instance (keyword (str (name comp-name) "/Lookup_E")) {:Id id})
        result (e/eval-all-dataflows evt)
        r (first result)]
    (when-not (= :not-found (:status r))
      (let [e (first (:result r))]
        (cn/same-instance? entity-instance e)))))

(deftest r01
  (defcomponent :EntityXformR01
    (entity :EntityXformR01/EPrime
            {:X :Kernel/Int})
    (event {:EntityXformR01/EToEPrime
            {:Instance :Kernel/Entity}})
    (dataflow :EntityXformR01/EToEPrime
              {:EntityXformR01/EPrime {:X :EntityXformR01/EToEPrime.Instance.X
                                       :Id :EntityXformR01/EToEPrime.Instance.Id}})
    (event {:EntityXformR01/EPrimeToE
            {:Instance :Kernel/Entity}}))
  (defcomponent :R01
    (entity {:R01/E {:X :Kernel/Int}}))
  (dataflow :EntityXformR01/EPrimeToE
            {:R01/E {:X :EntityXformR01/EPrimeToE.Instance.X
                     :Id :EntityXformR01/EPrimeToE.Instance.Id}})
  (let [e (cn/make-instance :R01/E {:X 10})
        result (tu/fresult (e/eval-all-dataflows {:R01/Upsert_E {:Instance e}}))
        e01 (first result)]
    (is (cn/instance-of? :R01/E e01))
    (is (nil? (second result)))
    (is (persisted? :R01 e01)))
  (compose-test-resolver :TestResolver01 :R01/E)
  (let [e (cn/make-instance :R01/E {:X 10})
        result (tu/fresult (e/eval-all-dataflows {:R01/Upsert_E {:Instance e}}))
        e01 (first result)]
    (is (cn/instance-of? :R01/E e01))
    (is (persisted? :R01 e01))
    (let [id (:Id e01)
          result (tu/fresult (e/eval-all-dataflows {:R01/Delete_E {:Id id}}))]
      (is (= result [[:R01 :E] id])))))

(defn- test-resolver-r02 [install-resolver resolver-name path]
  (let [f (fn [_ arg] arg)
        r (r/make-resolver resolver-name {:upsert {:handler identity
                                                   :xform {:in [f :EntityXformR02/EToE]
                                                           :out [f :EntityXformR02/EToK]}}
                                          :delete {:handler identity
                                                   :xform {:in [f]}}}
                           e/eval-pure-dataflows)]
    (install-resolver path r)))

(def compose-test-resolver-r02 (partial test-resolver-r02 rg/compose-resolver))
(def override-test-resolver-r02 (partial test-resolver-r02 rg/override-resolver))

(deftest r02
  (defcomponent :EntityXformR02
    (entity :EntityXformR02/E
            {:X :Kernel/Int})
    (event {:EntityXformR02/EToE
            {:Instance :Kernel/Entity}})
    (dataflow :EntityXformR02/EToE
              {:EntityXformR02/E
               {:X :EntityXformR02/EToE.Instance.X
                :Id :EntityXformR02/EToE.Instance.Id}})
    (event {:EntityXformR02/EToK
            {:Instance :Kernel/Entity}}))
  (defcomponent :R02
    (entity {:R02/E {:X :Kernel/Int}})
    (record {:R02/K {:X :Kernel/Int :Id :Kernel/UUID}}))
  (dataflow :EntityXformR02/EToK
            {:R02/K {:X :EntityXformR02/EToK.Instance.X
                     :Id :EntityXformR02/EToK.Instance.Id}})
  (override-test-resolver-r02 :TestResolver02 :R02/E)
  (let [e (cn/make-instance :R02/E {:X 10})
        result (tu/fresult (e/eval-all-dataflows {:R02/Upsert_E {:Instance e}}))
        e01 (first result)]
    (is (cn/instance-of? :R02/K e01))
    (is (not (persisted? :R02 e01)))))

(defn- test-query-resolver [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name
                           {:query
                            {:handler
                             (fn [arg]
                               (let [where-clause (:where (second arg))
                                     wild-card? (= where-clause :*)]
                                 (if wild-card?
                                   [(cn/make-instance :ResQueryAll/E {:X 1 :N "e01"})
                                    (cn/make-instance :ResQueryAll/E {:X 2 :N "e02"})]
                                   (when-let [id (nth (first where-clause) 2)]
                                     [(cn/make-instance :RQ/E {:X 1 :Id id})]))))}}
                           #(e/eval-all-dataflows % store {}))]
    (install-resolver path r)))

(deftest query
  (defcomponent :RQ
    (entity {:RQ/E {:X :Kernel/Int}}))
  (test-query-resolver rg/compose-resolver :RQResolver :RQ/E)
  (let [e (cn/make-instance :RQ/E {:X 10})
        e01 (first (tu/fresult (e/eval-all-dataflows {:RQ/Upsert_E {:Instance e}})))]
    (is (cn/instance-of? :RQ/E e01))
    (is (= 10 (:X e01)))
    (let [id (:Id e01)
          e02 (first (tu/fresult (e/eval-all-dataflows {:RQ/Lookup_E {:Id id}})))]
      (is (cn/instance-of? :RQ/E e02))
      ;(is (= id (:Id e02)))
      (is (= 1 (:X e02))))))

(deftest query-all
  (defcomponent :ResQueryAll
    (entity {:ResQueryAll/E {:X :Kernel/Int :N :Kernel/String}})
    (event {:ResQueryAll/AllE {}})
    (dataflow :ResQueryAll/AllE
              :ResQueryAll/E?))
  (test-query-resolver rg/compose-resolver :RQResolver :ResQueryAll/E)
  (let [es [(cn/make-instance :ResQueryAll/E {:X 1 :N "e01"})
            (cn/make-instance :ResQueryAll/E {:X 2 :N "e02"})]
        evts (map #(cn/make-instance :ResQueryAll/Upsert_E {:Instance %}) es)
        _ (doall (map tu/fresult (map #(e/eval-all-dataflows %) evts)))
        result (tu/fresult (e/eval-all-dataflows {:ResQueryAll/AllE {}}))]
    (doseq [r result]
      (is (cn/instance-of? :ResQueryAll/E r))
      (is (= (if (= 1 (:X r)) "e01" "e02") (:N r))))))
