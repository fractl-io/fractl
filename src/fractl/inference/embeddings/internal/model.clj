(ns fractl.inference.embeddings.internal.model
  (:require [fractl.util :as u]))

(defn object? [{classname :classname text-content :text_content
                meta-content :meta_content embedding :embedding
                embedding-model :embedding_model}]
  (and (string? classname)
       (string? text-content)
       (string? meta-content)
       (every? float? embedding)
       (if embedding-model (string? embedding-model) true)))

(defn as-object [attrs]
  (if (object? attrs)
    attrs
    (u/throw-ex (str "invalid embedding object: " attrs))))
