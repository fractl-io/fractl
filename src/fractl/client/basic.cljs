(ns fractl.client.basic
  (:require [clojure.walk :as w]
            [fractl.lang :refer [entity event]]
            [fractl.client.util :as u :refer-macros [defcomponent]]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(def view-tag :DOM_View)

(defcomponent :Fractl.Basic_UI
  (entity {:Fractl.Basic_UI/Component
           {:DOM_View {:listof :Kernel/Any
                       :default []}}})
  (event {:Fractl.Basic_UI/DomEvent
          {:Value :Kernel/String}}))

(defn- parse-dispatch-on
  [form]
  (let [[cmd event callback-f] form]
    (when (and cmd (= cmd :dispatch-on)
               event (= event :Fractl.Basic_UI/DomEvent)
               callback-f (fn? callback-f))
      [:Fractl.Basic_UI/DomEvent callback-f])))

(defn- rewrite-event [ev-name model-event]
  (let [[event-name callback-f] (parse-dispatch-on model-event)]
    (if (and event-name callback-f)
      [ev-name
       (fn [dom-evt]
         (let [value (-> dom-evt .-target .-value)
               evt-body {:Value value}]
           (callback-f :Fractl.Basic_UI/DomEvent evt-body)))]
      [ev-name nil])))

(def ^:private ui-event-names #{:on-click :on-change})

(defn- process-view [obj]
  (w/postwalk
   #(if (and (vector? %)
             (some #{(first %)} ui-event-names))
      (let [x (second %)]
        (if (fn? x)
          %
          (rewrite-event (first %) x)))
      %)
   obj))

(defn- inst->component
  [inst]
  (when-let [spec (view-tag inst)]
    (assoc inst view-tag (process-view spec))))

(defn- component-resolver-fn [install-resolver resolver-name path]
  (let [r (r/make-resolver resolver-name {:upsert {:handler inst->component}})]
    (println "component-resolver-fn - r: " r)
    (install-resolver path r)))

(def component-resolver-f (partial component-resolver-fn rg/compose-resolver))

(def comp-resolver (component-resolver-f :UIBasicResolver :Fractl.Basic_UI/Component))
