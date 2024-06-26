(ns fractl.inference.embeddings.model
  (:require [fractl.lang :refer [component entity]]
            [fractl.component :as cn]))

(component :Fractl.Inference.Embeddings)

(entity
 :Fractl.Inference.Embeddings/Object
 {:classname :String
  :text_content :String
  :meta_content :String
  :embedding {:listof :Float}
  :embedding_model {:type :String :optional true}})

(defn object? [x]
  (cn/instance-of? :Fractl.Inference.Embeddings/Object x))

(defn as-object [attrs]
  (cn/make-instance :Fractl.Inference.Embeddings/Object attrs))
