(ns fractl.resolver.camel
  (:require [clojure.string :as s]
            [fractl.lang :as ln]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.lang.internal :as li]
            [fractl.evaluator.camel :as camel]))

(ln/component :Camel)
(ln/record
 :Camel/EventTemplate
 {:UserArg :String
  :Response {:type :Any :optional true}})

(def ^:private component-register (atom {}))

(defn register-component [s f]
  (swap! component-register assoc (keyword s) f)
  s)

(defn- endpoint [event-name]
  (get-in (cn/fetch-meta event-name) [:trigger :endpoint]))

(defn- fetch-component [config endpoint]
  (let [n (first (s/split endpoint #":"))]
    (if-let [f (get @component-register (keyword n))]
      (f config)
      (do (log/warn (str "component not supported for endpoint - " endpoint)) nil))))

(defn camel-eval [config event-instance]
  (let [ep (endpoint (cn/instance-type event-instance))]
    (camel/exec-route-with-dataflow-callback
     {:endpoint ep
      :user-arg (:UserArg event-instance)
      :camel-component (fetch-component config ep)
      :callback #(assoc event-instance :Response %)})
    event-instance))

(defn- camel-on-set-path [_ [tag path]]
  (when-not (= tag :override)
    (u/throw-ex (str path " needs an override resolver")))
  (when-not (cn/event? path)
    (u/throw-ex (str path " is not an event")))
  (when-not (cn/inherits? :Camel/EventTemplate path)
    (u/throw-ex (str path " must inherit :Camel/EventTemplate")))
  (when-not (endpoint path)
    (u/throw-ex (str path " event does not define a trigger endpoint")))
  (when-not (cn/find-dataflows path)
    (u/throw-ex (str "no dataflow is defined for " path)))
  (let [[c n] (li/split-path path)
        trigger-event (li/make-path c (keyword (str "Trigger" (name n))))]
    (ln/dataflow
     trigger-event
     {path {:UserArg (li/make-ref trigger-event :UserArg)}})
    path))

(rg/defmake :camel
  (fn [resolver-name config]
    (r/make-resolver
     resolver-name
     {:eval (partial camel-eval config)
      :on-set-path (partial camel-on-set-path config)})))
