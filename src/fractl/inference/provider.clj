(ns fractl.inference.provider
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.inference.provider.protocol :as p]
            [fractl.inference.provider.model]
            [fractl.inference.provider.openai]
            [fractl.inference.provider.registry :as r]))

(defn- make-provider-request [pfn spec]
  (if-let [active-provider (r/fetch-active-provider)]
    (pfn active-provider spec)
    (u/throw-ex "No active LLM provider")))

(def make-embedding (partial make-provider-request p/make-embedding))

(defn- inference-agent? [x]
  (when x
    (cn/instance-of? :Fractl.Inference.Service/Agent x)))

(defn- preproc-messages [msgs]
  (mapv
   #(into
     {}
     (mapv
      (fn [[k v]]
        (let [k (u/string-as-keyword k)]
          [k (if (= k :role) (keyword v) v)]))
      %))
   msgs))

(defn- maybe-agent-to-spec [obj]
  (cond
    (inference-agent? obj)
    {:messages (preproc-messages (:Messages obj))}
    (inference-agent? (:agent obj))
    (assoc (dissoc obj :agent)
           :messages (preproc-messages (get-in obj [:agent :Messages])))
    :else obj))

(defn make-completion [spec]
  (make-provider-request p/make-completion (maybe-agent-to-spec spec)))

(def get-embedding (comp first make-embedding))
(def get-completion (comp first make-completion))
