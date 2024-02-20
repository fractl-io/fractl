(ns fractl.subs
  "Subsriptions for external events to handle out-of-bands change-notifications from
backend-systems. A resolver can express interest in these events by implementing the
:on-change-notification method."
  (:require [fractl.util :as u]
            [fractl.subs.kafka :as kafka]))

(def ^:private clients
  {:kafka {:open-connection kafka/make-consumer
           :listen kafka/listen
           :shutdown kafka/shutdown}})

(defn open-connection [config]
  (let [client-type (:type config)]
    (if-let [methods (client-type clients)]
      {:conn ((:open-connection methods) (dissoc config :type))
       :methods methods}
      (u/throw-ex (str "unsupported client-type: " client-type)))))

(defn- dispatch-method [tag client]
  ((get-in client [:methods tag]) (:conn client)))

(def listen (partial dispatch-method :listen))
(def shutdown (partial dispatch-method :shutdown))
