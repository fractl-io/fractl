(ns fractl.test.features04
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship
                     dataflow]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-917-child-identity
  (defcomponent :I917
    (entity
     :I917/P
     {:A {:type :Int
          :identity true}})
    (entity
     :I917/C
     ;; :X is user-defined identity, turned to indexed.
     ;; :Path and the unique-key must be
     ;; defined by the compiler.
     {:X {:type :Int
          :identity true}
      :Y :Int})
    (relationship
     :I917/R
     ;; :on should be added by compiler,
     ;; this means the whole transformation described here
     ;; should happen only if a user-specified `:on` is not
     ;; present in the relationship.
     {:meta {:contains [:I917/P :I917/C]
             :with-paths true}}))
  (let [p? (partial cn/instance-of? :I917/P)
        c? (partial cn/instance-of? :I917/C)
        ps (mapv #(tu/first-result
                   {:I917/Create_P
                    {:Instance
                     {:I917/P
                      {:A %}}}})
                 [1 2])
        c0 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 20 :Y 100}}
              :P 1}})
        c1 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 10 :Y 100}}
              :P 1}})
        c2 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 10 :Y 200}}
              :P 2}})
        c3 (tu/result
            {:I917/Create_C
             {:Instance
              {:I917/C
               {:X 10 :Y 300}}
              :P 1}})]
    (is (every? p? ps))
    (is (every? c? [c0 c1 c2 c3]))
    (is (= (li/path-attr c0) "/P/1/R/C/20"))
    (is (= (li/path-attr c2) "/P/2/R/C/10"))
    ;; c3 did nothing because of idempotent create
    (is (= 2 (count (tu/result
                     {:I917/LookupAll_C
                      {:P 1}}))))
    (is (= 1 (count (tu/result
                     {:I917/LookupAll_C
                      {:P 2}}))))))
