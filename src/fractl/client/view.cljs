(ns fractl.client.view
  (:require [clojure.walk :as w]
            [fractl.lang :refer [entity]]
            [fractl.client.util :as u :refer-macros [defcomponent]]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rr]
            [fractl.store.reagent.core :as rstore]
            [reagent.dom :as rgdom]
            [reagent.core :as rg]))

(defcomponent :Fractl.View
  (entity {:Fractl.View/Root
           {:DOM_View {:listof :Kernel/Any
                       :default []}
            :DOM_Target :Kernel/String}}))

(def view-tag :DOM_View)
(def target-tag :DOM_Target)

(defn- view-track? [x]
  (and (seqable? x) (= :view-track (first x))))

(defn- track? [x]
  (and (seqable? x) (= :track (first x))))

(defn- tracker [x]
  (first (rest x)))

(defn- process-view [spec]
  (w/postwalk
   #(cond
      (view-track? %)
      (let [path (tracker %)
            inst-ref-view (deref (rg/cursor rstore/state path))]
        (process-view inst-ref-view))

      (track? %)
      (let [path (tracker %)]
        (deref (rg/cursor rstore/state path)))

      :else
      %)
   spec))

(defn- upsert-view
  [view-inst]
  (rgdom/render
   [(fn [] (process-view (view-tag view-inst)))]
   (.getElementById js/document (target-tag view-inst)))
  view-inst)

(defn- view-resolver-fn [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name {:upsert {:handler upsert-view}})]
    (install-resolver path r)))

(def view-resolver-f (partial view-resolver-fn rr/override-resolver))

(def view-resolver (view-resolver-f :UIViewResolver :Fractl.View/Root))
