(ns fractl.inference.embeddings.internal.queue
  (:require [clojure.core.async :as async]
            [fractl.util :as u])
  (:import [java.util.concurrent ConcurrentLinkedQueue]))

(def ^:private ^ConcurrentLinkedQueue q (ConcurrentLinkedQueue.))

(defn process [handler]
  (async/thread
    (loop [obj (.poll q)]
      (if obj
        (do (handler obj)
            (recur (.poll q)))
        (do (Thread/sleep 5000)
            (recur (.poll q)))))))

(defn enqueue [obj]
  (.add q obj)
  obj)
