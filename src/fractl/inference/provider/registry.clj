(ns fractl.inference.provider.registry
  (:require [fractl.util :as u]))

(def ^:private registry (atom {}))

(defn register-provider [provider-name provider-impl]
  (swap! registry assoc (u/string-as-keyword provider-name) provider-impl))

(defn fetch-provider [provider-name]
  (get @registry (u/string-as-keyword provider-name)))
