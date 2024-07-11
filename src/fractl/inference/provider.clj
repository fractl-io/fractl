(ns fractl.inference.provider
  (:require [fractl.util :as u]
            [fractl.inference.provider.protocol :as p]
            [fractl.inference.provider.model]
            [fractl.inference.provider.openai]
            [fractl.inference.provider.registry :as r]))

(defn- make-provider-request [pfn spec]
  (if-let [active-provider (r/fetch-active-provider)]
    (pfn active-provider spec)
    (u/throw-ex "No active LLM provider")))

(def make-embedding (partial make-provider-request p/make-embedding))
(def make-completion (partial make-provider-request p/make-completion))

(def get-embedding (comp first make-embedding))
(def get-completion (comp first make-completion))
