(ns fractl.inference.provider.registry
  (:require [fractl.util :as u]
            [fractl.inference.provider.core :as c]))

(def ^:private registry (atom {}))

(defn register-provider [provider-type provider-impl]
  (swap! registry assoc (u/string-as-keyword provider-type) provider-impl))

(defn fetch-provider [provider-type]
  (get @registry (u/string-as-keyword provider-type)))

(defn fetch-active-provider []
  (when c/active-llm
    (fetch-provider (:Type c/active-llm))))

(defn fetch-active-provider-config []
  (:Config c/active-llm))
