(ns fractl.inference.provider.core
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]))

(def ^:dynamic active-llm nil)

(def lookup-provider-by-name
  (memoize
   (fn [provider-name]
     (when provider-name
       (first (ev/safe-eval-internal
               false {:Fractl.Inference.Provider/FindLLM
                      {:Name provider-name}}))))))

(defn call-with-provider [provider-name f]
  (if-let [provider (lookup-provider-by-name provider-name)]
    (binding [active-llm provider]
      (f))
    (u/throw-ex (str "LLM provider " provider-name " not found"))))
