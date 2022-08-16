(ns fractl.test.fixes03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-576-alias
  (defcomponent :I576
    (entity
     {:I576/E
      {:X :Kernel/Int}})
    (record
     {:I576/R
      {:A {:listof :I576/E}
       :B :Kernel/Int
       :C {:listof :I576/E}}})
    (dataflow
     :I576/Evt
     {:I576/E? {} :as [:Result
                       [:E1 :_ :_ :E4 :& :Es]]}
     {:I576/R
      {:A :Result
       :B '(+ :E1.X :E4.X)
       :C :Es}}))
  (let [xs (range 1 11)
        sum (apply + xs)
        rs1 (mapv #(tu/first-result
                    {:I576/Upsert_E
                     {:Instance
                      {:I576/E
                       {:X %}}}})
                  xs)
        r (tu/first-result {:I576/Evt {}})
        e? (partial cn/instance-of? :I576/E)]
    (is (every? e? rs1))
    (is (= sum (apply + (mapv :X rs1))))
    (is (cn/instance-of? :I576/R r))
    (is (every? e? (:A r)))
    (is (= sum (apply + (mapv :X (:A r)))))
    (is (= 5 (:B r)))
    (is (every? e? (:C r)))
    (is (= 6 (count (:C r))))))

(deftest issue-585-eval
  (defcomponent :I585
    (entity
     :I585/E
     {:K {:type :Kernel/String
          :indexed true}
      :X :Kernel/Int})
    (record
     :I585/R
     {:Y :Kernel/Int})
    (defn i585-f1 [e]
      (cn/make-instance
       :I585/R
       {:Y (* (:X e) 200)}))
    (defn i585-f2 [e]
      [(cn/make-instance
        :I585/R
        {:Y (* (:X e) 10)})])
    (defn i585-seq-of-r? [xs]
      (every? (partial cn/instance-of? :I585/R) xs))
    (dataflow
     :I585/Evt1
     {:I585/E {:K? :I585/Evt1.K}}
     [:eval '(fractl.test.fixes03/i585-f1 :I585/E)
      :check :I585/R :as :Result]
     {:I585/E {:K "result" :X :Result.Y}})
    (dataflow
     :I585/Evt2
     {:I585/E {:K? :I585/Evt2.K}}
     [:eval '(fractl.test.fixes03/i585-f2 :I585/E)
      :check fractl.test.fixes03/i585-seq-of-r? :as [:R1]]
     {:I585/E {:K "result" :X :R1.Y}})
    (dataflow
     :I585/Evt3
     {:I585/E {:K? :I585/Evt3.K}}
     [:eval '(fractl.test.fixes03/i585-f1 :I585/E)]))
  (let [e1 (tu/first-result
            {:I585/Upsert_E
             {:Instance
              {:I585/E {:K "abc" :X 10}}}})
        r1 (tu/first-result
            {:I585/Evt1 {:K "abc"}})
        r2 (tu/first-result
            {:I585/Evt2 {:K "abc"}})
        r3 (tu/fresult
            (tu/eval-all-dataflows
             {:I585/Evt3 {:K "abc"}}))]
    (is (cn/instance-of? :I585/E r1))
    (is (= 2000 (:X r1)))
    (is (cn/instance-of? :I585/E r2))
    (is (= 100 (:X r2)))
    (is (cn/instance-of? :I585/R r3))
    (is (= 2000 (:Y r3)))))
