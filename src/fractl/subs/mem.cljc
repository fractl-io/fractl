(ns fractl.subs.mem
  "An in-memory pub/sub for testing purposes."
  (:require [fractl.subs.internal :as si]))

(defn open [config] {:data (:data config)})

(defn listen [conn transform]
  (doseq [record (:data conn)]
    (si/process-notification (transform record))))

(defn shutdown [_] {:data nil})
