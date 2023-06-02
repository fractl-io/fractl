(ns fractl.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.meta :as mt]
            [fractl.policy :as policy]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(defn- fetch-spec [x]
  (second (policy/spec (first x))))

(deftest issue-506-policy-inheritance
  (#?(:clj do
      :cljs cljs.core.async/go)
   (mt/set-policy-parser! mt/views-tag policy/create-policy)
   (defcomponent :I506PI
     (entity
      :I506PI/E1
      {:X :Int
       :meta
       {:views {:style {:background :red}}}})
     (entity
      :I506PI/E2
      {:Y :Int})
     (entity
      :I506PI/E3
      {:Z :Int
       :meta
       {:contains [:I506PI/E1 :I506PI/E2]
        :views {:style {:background :white}}}}))
   (let [p1 (policy/lookup-policies :views :I506PI/E1)
         p2 (policy/lookup-policies :views :I506PI/E2)
         p3 (policy/lookup-policies :views :I506PI/E3)
         path [:style :background]]
     (is (= :red (get-in (fetch-spec p1) path)))
     (is (= (fetch-spec p2) (fetch-spec p3))))))
