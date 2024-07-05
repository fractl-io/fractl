(ns fractl.inference.embeddings.internal.queue
  (:require [clojure.core.async :as async]
            [fractl.util.logger :as log])
  (:import [java.util.concurrent ConcurrentLinkedQueue]))

(def ^:private ^ConcurrentLinkedQueue q (ConcurrentLinkedQueue.))

(defn- wait-to-poll! []
  (try
    (Thread/sleep 5000)
    (catch InterruptedException _)))

(defn process [handler]
  (async/thread
    (do (wait-to-poll!)
        (loop [obj (.poll q)]
          (if obj
            (try
              (handler obj)
              (catch Exception ex
                (log/error ex)))
            (wait-to-poll!))
          (recur (.poll q))))))

(defn enqueue [obj]
  (.add q obj)
  obj)
