(ns agentlang.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.evaluator :as e]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.meta :as mt]
            [agentlang.policy :as policy]
            [agentlang.lang.internal :as li]
            [agentlang.lang.datetime :as dt]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(defn- fetch-spec [x]
  (second (policy/spec (first x))))
