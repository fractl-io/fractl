(ns agentlang.subs.internal
  (:require [agentlang.util :as u]
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry :as rg]))

(defn- operation? [x]
  (when (some #{(u/string-as-keyword x)} [:create :update :delete])
    true))

(defn notification-object
  ([opr inst old-inst]
   (when-not (operation? opr)
     (u/throw-ex (str "invalid operation - " opr)))
   {:operation opr
    :instance (cn/make-instance inst)
    :old-instance (when old-inst (cn/make-instance inst))})
  ([opr inst] (notification-object opr inst nil)))

(defn notification-object? [obj]
  (and (map? obj)
       (map? (:instance obj))
       (operation? (:operation obj))))

(defn normalize-notification-object [obj]
  (let [instance (:instance obj)
        old-instance (:old-instance obj)
        opr (u/string-as-keyword (:operation obj))]
      (notification-object opr instance old-instance)))

(def connection :conn)
(def _methods :methods)
(def xform :xform)
(def _filter :filter)
(def open-connection :open-connection)
(def listen :listen)
(def shutdown :shutdown)

(defn make-client [open-conn-fn listen-fn shutdown-fn]
  {open-connection open-conn-fn
   listen listen-fn
   shutdown shutdown-fn})

(defn call-listen [client]
  ((get-in client [_methods listen]) client))

(defn call-shutdown [client]
  ((get-in client [_methods shutdown]) client))

(defn call-transformer [client obj]
  ((get-in client [_methods xform]) obj))

(defn call-filter [client obj]
  ((get-in client [_methods _filter]) obj))

(defn with-fn [tag f client]
  (let [methods (_methods client)]
    (assoc client _methods (assoc methods tag f))))

(defn- maybe-add-methods [tag default methods]
  (if (tag methods)
    methods
    (assoc methods tag default)))

(def ^:private maybe-add-xform (partial maybe-add-methods xform normalize-notification-object))
(def ^:private maybe-add-filter (partial maybe-add-methods _filter identity))

(defn- add-default-methods [methods]
  (-> methods maybe-add-xform maybe-add-filter))

(defn make-client-connection [conn-obj methods]
  {connection conn-obj
   _methods (add-default-methods methods)})

(defn process-notification [client arg]
  (let [obj (call-transformer client arg)]
    (when (call-filter client obj)
      (when-let [r (rg/resolver-for-path
                    (let [inst (:instance obj)]
                      (or (cn/instance-type inst)
                          (li/split-path (li/record-name inst)))))]
        (r/call-resolver-on-change-notification r nil obj)))))
