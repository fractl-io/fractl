(ns fractl.test.features05
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.pprint :as pp]
            [fractl.component :as cn]
            [fractl.env :as env]
            [fractl.lang
             :refer [component event entity relationship dataflow rule inference]]
            [fractl.lang.raw :as lr]
            [fractl.lang.internal :as li]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest rule-basic
  (defcomponent :Rule01
    (entity :Rule01/A {:Id :Identity :X :Int})
    (entity :Rule01/B {:Id :Identity :Y :Int})
    (rule
     :Rule01/R1
     {:Rule01/A {:X 100} :as :A}
     {:Rule01/B {:Y [:<= 200]} :as :B}
     :then
     {:Rule01/Event1 {:A :A.Id}}
     {:Rule01/Event2 {:B :B.Id}})
    (rule
     :Rule01/R2
     {:Rule01/A {:X [:or [:= 0] [:= 100]]} :as :A}
     :then
     {:Rule01/Event3 {:A :A.Id}}
     {:meta {:priority 10 :passive true :category :Rule01.Abc}}))
  (let [spec (cn/fetch-rule :Rule01/R1)]
    (is (= [{:Rule01/A {:X 100} :as :A}
            {:Rule01/B {:Y [:<= 200]} :as :B}]
           (cn/rule-condition spec)))
    (is (= [{:Rule01/Event1 {:A :A.Id}}
            {:Rule01/Event2 {:B :B.Id}}]
           (cn/rule-consequence spec)))
    (is (cn/rule-has-least-priority? spec))
    (is (not (cn/rule-is-passive? spec)))
    (is (not (cn/rule-category spec))))
  (let [spec (cn/fetch-rule :Rule01/R2)]
    (is (= [{:Rule01/A {:X [:or [:= 0] [:= 100]]} :as :A}]
           (cn/rule-condition spec)))
    (is (= [{:Rule01/Event3 {:A :A.Id}}]
           (cn/rule-consequence spec)))
    (is (= 10 (cn/rule-priority spec)))
    (is (cn/rule-is-passive? spec))
    (is (= :Rule01.Abc (cn/rule-category spec)))))

(deftest rule-fire-01
  (defcomponent :Rf01
    (entity :Rf01/A {:Id :Identity :X :Int})
    (entity :Rf01/B {:Id :Identity :Y :Int :A :UUID})
    (dataflow :Rf01/BbyA {:Rf01/B {:A? :Rf01/BbyA.A}})
    (rule
     :Rf01/R1
     {:Rf01/A {:X 100} :as :InstA}
     :then
     {:Rf01/B {:Y 100 :A :InstA.Id}})
    (rule
     :Rf01/R2
     {:Rf01/A {:X [:>= 500]} :as :InstA}
     :then
     {:Rf01/B {:Y '(* 100 :InstA.X) :A :InstA.Id}})
    (rule
     :Rf01/R3
     [:delete {:Rf01/A {:X 100}}]
     :then
     [:delete :Rf01/B {:A :Rf01/A.Id}]))
  (let [make-a (fn [x]
                 (let [r (first
                          (tu/eval-all-dataflows
                           {:Rf01/Create_A
                            {:Instance
                             {:Rf01/A {:X x}}}}))]
                   [(:env r) (first (:result r))]))
        delete-a (fn [a]
                   (let [r (first
                            (tu/eval-all-dataflows
                             {:Rf01/Delete_A {:Id (:Id a)}}))]
                     [(:env r) (first (:result r))]))
        [[env1 a1] [env2 a2]] (mapv make-a [10 100])
        a? (partial cn/instance-of? :Rf01/A)
        b? (partial cn/instance-of? :Rf01/B)
        b-by-a (fn [a] (tu/eval-all-dataflows {:Rf01/BbyA {:A (:Id a)}}))
        is-no-b-by-a (fn [a] (= :not-found (:status (first (b-by-a a)))))
        is-b-by-a (fn [rname a]
                    (let [bs (:result (first (b-by-a a)))]
                      (is (every? b? bs))
                      (is (= 1 (count bs)))
                      (if (= rname :R2)
                        (is (= (* 100 (:X a)) (:Y (first bs))))
                        (is (= 100 (:Y (first bs)))))))
        is-b-in-env (fn [env]
                      (is (b? (first (:result (first (deref (first (env/rule-futures env)))))))))]
    (is (every? a? [a1 a2]))
    (is (nil? (seq (env/rule-futures env1))))
    (is-b-in-env env2)
    (is-no-b-by-a a1)
    (is-b-by-a :R1 a2)
    (let [[env3 a3] (make-a 500), [env4 a4] (make-a 501)]
      (is (every? a? [a3 a4]))
      (is-b-in-env env3)
      (is-b-in-env env4)
      (is-b-by-a :R2 a3)
      (is-b-by-a :R2 a4)
      (let [[env a] (delete-a a4)]
        (is (cn/same-instance? a a4))
        (is (nil? (seq (env/rule-futures env)))))
      (is-b-by-a :R2 a4)
      (is-b-by-a :R1 a2)
      (let [[env a] (delete-a a2)]
        (is (cn/same-instance? a a2))
        (let [bs (:result (first (deref (first (env/rule-futures env)))))]
          (is (= 1 (count bs)))
          (is (= 100 (:Y (first bs))))
          (is (= (:Id a2) (:A (first bs)))))
        (is-no-b-by-a a2)))))

(deftest issue-1252-rules-inference-raw-syntax
  (defcomponent :I1252R
    (entity :I1252R/A {:Id :Identity :X :Int})
    (entity :I1252R/B {:Id :Identity :Y :Int})
    (rule
     :I1252R/R1
     {:I1252R/A {:X 10}}
     :then
     {:I1252R/B {:Y 200}}
     {:meta {:priority 1}})
    (inference
     :I1252R/I1
     {:category :I1Rules
      :seed []
      :embed [:I1252R/A :I1252R/B]}))
  (is (= (lr/as-edn :I1252R)
         '(do
            (component :I1252R)
            (entity :I1252R/A {:Id :Identity, :X :Int})
            (entity :I1252R/B {:Id :Identity, :Y :Int})
            (rule
             :I1252R/R1
             #:I1252R{:A {:X 10}}
             :then
             #:I1252R{:B {:Y 200}}
             {:meta {:priority 1}})
            (inference
             :I1252R/I1
             {:category :I1Rules, :seed [], :embed [:I1252R/A :I1252R/B]})))))

(deftest issue-1300-joins
  (defcomponent :I1300J
    (entity
     :I1300J/Customer
     {:Id {:type :Int :guid true}
      :Name :String})
    (entity
     :I1300J/Order
     {:Id {:type :Int :guid true}
      :CustomerId :Int
      :Date :Now})
    (dataflow
     :I1300J/CustomerOrders
     {:I1300J/Order? {}
      :join [{:I1300J/Customer {:Id? :I1300J/Order.CustomerId}}]
      :with-attributes {:CustomerName :I1300J/Customer.Name
                        :CustomerId :I1300J/Customer.Id
                        :OrderId :I1300J/Order.Id}}))
  (let [cust (fn [id name]
               (tu/first-result
                {:I1300J/Create_Customer
                 {:Instance
                  {:I1300J/Customer {:Id id :Name name}}}}))
        cust? (partial cn/instance-of? :I1300J/Customer)
        order (fn [id cust-id]
                (tu/first-result
                 {:I1300J/Create_Order
                  {:Instance
                   {:I1300J/Order {:Id id :CustomerId cust-id}}}}))
        order? (partial cn/instance-of? :I1300J/Order)
        cs (mapv cust [1001 1002 1003] ["jay" "mat" "joe"])
        _ (is (every? cust? cs))
        os (mapv order [1 2 3 4 5] [1001 1002 1001 1003 1003])
        _ (is (every? order? os))
        rs (tu/result {:I1300J/CustomerOrders {}})]
    (is (and (= 5 (count rs)) (is (every? map? rs))))
    (let [rs1 (filter #(= 1001 (:CustomerId %)) rs)
          p? (fn [ordid] (is (= 1 (count (filter #(= ordid (:OrderId %)) rs1)))))]
      (is (= 2 (count rs1)))
      (p? 1)
      (p? 3))))
