(ns agentlang.telemetry
  (:require [agentlang.global-state :as gs])
  (:import [java.util Queue]
           [java.util.concurrent ConcurrentLinkedQueue]))

(def ^:private state (atom nil))

(defn enabled? [] (and (gs/telemetry-config) true))

(defn- push-telemetry! [endpoint event-instance dataflow-result]
  )

(defn- start-polling! [config]
  (let [endpoint (:endpoint config)
        ^Queue q (:queue @state)]
    (loop [[event-instance dataflow-result :as r] (.poll q)]
      (if r
        (do (push-telemetry! endpoint event-instance dataflow-result)
            (recur (.poll q)))
        (do (try
              (Thread/sleep 1000)
              (catch Exception _ nil))
            (recur (.poll q)))))))
  
(defn init [config]
  (swap! state assoc :queue (ConcurrentLinkedQueue.) :config config)
  (.start (Thread. #(start-polling! config)))
  config)
    
(defn publish! [event-instance dataflow-result]
  (let [^Queue q (:queue @state)]
    (.add q [event-instance dataflow-result])))
