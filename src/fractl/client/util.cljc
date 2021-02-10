(ns fractl.client.util
  (:require [fractl.store :as store]
            [fractl.evaluator :as e]))

#?(:clj
   (def store (store/open-default-store))
   :cljs
   (def store (store/open-default-store {:type :reagent :reactive true})))

(defn eval-dataflows-for-event
  [evt]
  (e/eval-all-dataflows evt store nil))

(defn fresult [r]
  (:result (first r)))

(defmacro defcomponent [component & body]
  `(do (fractl.lang/component ~component)
       ~@body
       ~component))
