(ns fractl.inference.provider
  (:require [fractl.util :as u]
            [fractl.inference.provider.protocol :as p]
            [fractl.inference.provider.model :as model]
            [fractl.inference.provider.registry :as r]))

(def ^:dynamic current-provider nil)

(defn- active-provider []
  (or current-provider (:Provider (model/fetch-config))))

(defn- make-provider-request [pfn spec]
  (if-let [provider-name (active-provider)]
    (if-let [provider (r/fetch-provider provider-name)]
      (pfn provider spec)
      (u/throw-ex (str "LLM provider " provider-name " is not registered")))
    (u/throw-ex "No active LLM provider")))

(def make-embedding (partial make-provider-request p/make-embedding))
(def make-completion (partial make-provider-request p/make-completion))

(def get-embedding (comp first make-embedding))
(def get-completion (comp first make-completion))
