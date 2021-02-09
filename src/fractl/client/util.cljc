(ns fractl.client.util
  (:require [fractl.util :as u]
            [fractl.store :as store]
            [fractl.lang.internal :as li]
            [fractl.evaluator :as e]
            [fractl.component :as cn]))

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

(defn dispatch
  [ev-name inst]
  (let [inst-name (cn/instance-name inst)
        [component _] (li/split-path inst-name)
        evt-name (keyword (str (name component) "/" (name ev-name)))
        evt (cn/make-instance evt-name {:Instance inst})]
    (when-not (fresult (e/eval-all-dataflows evt))
      (u/throw-ex (str "Failed to dispatch " ev-name " for " inst)))))
  