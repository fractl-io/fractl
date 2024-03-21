(ns fractl.subs
  "Subsriptions for external events to handle out-of-bands change-notifications from
backend-systems. A resolver can express interest in these events by implementing the
:on-change-notification method."
  (:require [fractl.util :as u]
            [fractl.subs.internal :as si]
            [fractl.subs.mem :as mem]
            [fractl.subs.kafka :as kafka]))

(def ^:private clients
  (atom {:kafka (si/make-client kafka/make-consumer kafka/listen kafka/shutdown)
         :mem (si/make-client mem/open mem/listen mem/shutdown)}))

(defn register-client [name-tag open-fn listen-fn shutdown-fn]
  (swap! clients assoc name-tag (si/make-client open-fn listen-fn shutdown-fn))
  name-tag)

(defn open-connection [config]
  (let [client-type (:type config)]
    (if-let [methods (client-type @clients)]
      (si/make-client-connection
       ((si/open-connection methods) (dissoc config :type))
       (merge methods (:methods config)))
      (u/throw-ex (str "unsupported client-type: " client-type)))))

(def notification-object si/notification-object)
(def notification-object? si/notification-object?)

;; Enrich a subscription-client with a transformer-function.
;; The transformer-fn will take an arbitrary object as argument
;; and translate that to a notification-object.
(def with-transformer (partial si/with-fn si/xform))

;; Enrich a subscription-client with a filter-predicate.
;; The predicate will take a notification-object as argument.
;; Only if the result is true, the notification-object is passed on to
;; the change-notification handler of the resolver.
(def with-filter (partial si/with-fn si/_filter))

(def listen si/call-listen)
(def shutdown si/call-shutdown)
