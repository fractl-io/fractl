(ns fractl.inference.embeddings
  (:require [fractl.inference.embeddings.internal.queue :as q]))

(defn embed-schema [spec] (q/enqueue spec))
