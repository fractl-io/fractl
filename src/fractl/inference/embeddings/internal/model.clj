(ns fractl.inference.embeddings.internal.model
  (:require [fractl.util :as u]))

(defn object? [{classname :classname text-content :text-content
                meta-content :meta-content embedding :embedding
                embedding-model :embedding-model}]
  (and (string? classname)
       (if text-content (string? text-content) true)
       (if meta-content (string? meta-content) true)
       (float? (first embedding)) ; (every? float? embedding) can be expensive
       (if embedding-model (string? embedding-model) true)))

(defn as-object [attrs]
  (if (object? attrs)
    attrs
    (u/throw-ex (str "invalid embedding object: " attrs))))
