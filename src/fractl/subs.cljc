(ns fractl.subs
  "Subsriptions for external events to handle out-of-bands change-notifications from
backend-systems. A resolver can express interest in these events by implementing the
:on-change-notification method."
  (:require [fractl.util :as u]
            [fractl.subs.internal :as si]
            [fractl.subs.mem :as mem]
            [fractl.subs.kafka :as kafka]))

(def ^:private clients
  {:kafka {:open-connection kafka/make-consumer
           :listen kafka/listen
           :shutdown kafka/shutdown}
   :mem {:open-connection mem/open
         :listen mem/listen
         :shutdown mem/shutdown}})

(defn- maybe-add-xform [methods]
  (if (:xform methods)
    methods
    (assoc methods :xform si/normalize-notification-object)))

(defn open-connection [config]
  (let [client-type (:type config)]
    (if-let [methods (client-type clients)]
      {:conn ((:open-connection methods) (dissoc config :type))
       :methods (maybe-add-xform methods)}
      (u/throw-ex (str "unsupported client-type: " client-type)))))

(def notification-object si/notification-object)
(def notification-object? si/notification-object?)

;; The transformer-fn will take an arbitrary object as argument
;; and return a notification-object.
(defn attach-transformer [client transformer-fn]
  (let [methods (:methods client)]
    (assoc client :methods (assoc methods :xform transformer-fn))))

(defn listen [client]
  ((get-in client [:methods :listen])
   (:conn client) (get-in client [:methods :xform])))

(defn shutdown [client]
  ((get-in client [:methods :shutdown]) (:conn client)))
