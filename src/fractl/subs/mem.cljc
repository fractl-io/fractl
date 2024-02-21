(ns fractl.subs.mem
  "An in-memory pub/sub for testing purposes."
  (:require [fractl.subs.internal :as si]))

(defn open [config] {:data (:data config)})

(defn listen [client]
  (doseq [record (:data (si/connection client))]
    (si/process-notification client record)))

(defn shutdown [_] {:data nil})
