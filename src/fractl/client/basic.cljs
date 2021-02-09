(ns fractl.client.basic
  (:require [clojure.walk :as w]
            [fractl.lang :refer [entity event]]
            [fractl.client.util :as u :refer-macros [defcomponent]]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [reagent.core :as reagent]))

(def view-tag :DOM_View)

(defcomponent :Fractl.Basic_UI
  (entity {:Fractl.Basic_UI/Component
           {:DOM_View {:listof :Kernel/Any
                       :default []}}})
  (event {:Fractl.Basic_UI/DomEvent
          {:Value :Kernel/String}}))

(defn- cursor-spec? [x]
  (and (seqable? x) (= :cursor (first x))))

(defn- cursor-path [x] (first (rest x)))

(defn- process-data
  [spec]
  (reduce-kv (fn [m k v]
               (assoc m k (cond
                            (cursor-spec? v)
                            @(reagent/track (cursor-path v))

                            (and (keyword? v) (= :value v))
                            v

                            :else
                            v)))
             {}
             spec))

(defn- process-view [spec]
  (println "process-cursors - spec: " spec)
  (into [] (map #(cond
                   (cursor-spec? %)
                   (cursor-path %)

                   (vector? %)
                   (process-view %)

                   (map? %)
                   (process-data %)

                   (and (keyword? %) (= :value %))
                   %

                   :else
                   %)
                spec)))

(defn- process-view-2 [spec]
  (w/postwalk
   #(if (cursor-spec? %)
      [(cursor-path %)]
      %)
   spec))

(defn- inst->component
  [inst]
  (println "inst->component - inst: " inst)
  inst
  #_(when-let [spec (view-tag inst)]
    (assoc inst view-tag (process-view spec))))

(defn- component-resolver-fn [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name {:upsert {:handler inst->component}})]
    (println "component-resolver-fn - r: " r)
    (install-resolver path r)))

(def component-resolver-f (partial component-resolver-fn rg/compose-resolver))

(def comp-resolver (component-resolver-f :UIBasicResolver :Fractl.Basic_UI/Component))
