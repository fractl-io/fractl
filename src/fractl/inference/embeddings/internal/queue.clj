(ns fractl.inference.embeddings.internal.queue
  (:require [clojure.core.async :as async]
            [fractl.util.logger :as log])
  (:import [java.util.concurrent ConcurrentLinkedQueue]))

(def ^:private ^ConcurrentLinkedQueue q (ConcurrentLinkedQueue.))

(defn process [handler]
  (async/thread
    (loop [obj (.poll q)]
      (if obj
        (try
          (handler obj)
          (catch Exception ex
            (log/error ex)))
        (try
          (Thread/sleep 5000)
          (catch InterruptedException _
            )))
      (recur (.poll q)))))

(defn enqueue [obj]
  (.add q obj)
  obj)
