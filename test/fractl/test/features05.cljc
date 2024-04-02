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

(deftest issue-1255-rules-with-contains
  (defcomponent :I1255
    (entity
     :I1255/Customer
     {:Email {:type :Email :guid true}
      :Type {:oneof ["primary" "normal"]}})
    (entity
     :I1255/SupportTicket
     {:No {:type :Int :id true}
      :Created :Now
      :Priority {:oneof ["high" "low"]}})
    (relationship
     :I1255/CustomerSupportTicket
     {:meta {:contains [:I1255/Customer :I1255/SupportTicket]}})
    (entity
     :I1255/SupportExecutiveAssignment
     {:SupportExecutiveId {:type :Int :expr '(rand-int 100)}
      :TicketNo :Int
      :CustomerEmail :Email})
    (rule
     :I1255/HandlePrimaryTicket
     {:I1255/SupportTicket {:Priority "high"}
      ;;:-> [[:I1255/CustomerSupportTicket {:I1255/Customer {:Type "primary"}}]]
      :as :ST}
     :then
     {:I1255/SupportExecutiveAssignment
      {:TicketNo :ST.No
       :CustomerEmail "b@acme.com"#_:I1255/Customer.Email}}))
  (let [mk-cust (fn [email type]
                  (let [c (tu/first-result
                           {:I1255/Create_Customer
                            {:Instance
                             {:I1255/Customer
                              {:Email email :Type (name type)}}}})]
                    (is (cn/instance-of? :I1255/Customer c))
                    c))
        [c1 c2] (mapv mk-cust ["a@acme.com" "b@acme.com"] [:normal :primary])
        mk-tkt (fn [cust-email no prio]
                 (let [r (first
                          (tu/eval-all-dataflows
                           {:I1255/Create_SupportTicket
                            {:Instance
                             {:I1255/SupportTicket
                              {:No no :Priority (name prio)}}
                             li/path-attr (str "/Customer/" cust-email "/CustomerSupportTicket")}}))]
                   (when (= :ok (:status r))
                     (let [sp (first (:result r))]
                       (is (cn/instance-of? :I1255/SupportTicket sp))
                       [(:env r) sp]))))
        [[env1 sp1] [env2 sp2]] (mapv mk-tkt ["a@acme.com" "b@acme.com"] [1 2] [:low :high])
        f1 (seq (env/rule-futures env1))
        f2 (seq (env/rule-futures env2))]
    (is (nil? f1))
    #_(println (deref (first f2)))
    #_(println (tu/result {:I1255/LookupAll_SupportExecutiveAssignment {}}))))
