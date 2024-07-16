(ns fractl.inference.provider
  (:require [fractl.util :as u]
            [fractl.inference.provider.protocol :as p]
            [fractl.inference.provider.model :as model]
            [fractl.inference.provider.registry :as r]))

(defn- active-provider []  
  (:Provider (model/fetch-config)))

(defn make-embedding [spec]
  (if-let [provider (r/fetch-provider (active-provider))]
    (p/make-embedding provider spec)
    (u/throw-ex "make-embedding failed, no provider registered")))

(defn make-completion [spec]
  (if-let [provider (r/fetch-provider (active-provider))]
    (p/make-completion provider spec)
    (u/throw-ex "make-completion failed, no provider registered")))

(def get-embedding (comp first make-embedding))
(def get-completion (comp first make-completion))
