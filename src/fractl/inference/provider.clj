(ns fractl.inference.provider
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.inference.service.model :as model]
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

(defn- add-user-instruction [agent-instance msgs]
  (if-let [ins (or (:UserInstruction agent-instance)
                   (get-in agent-instance [:Context :UserInstruction]))]
    (vec (concat msgs [{:role :user :content ins}]))
    msgs))

(defn- fetch-messages [agent-instance]
  (when-let [sess (model/lookup-agent-chat-session agent-instance)]
    [(add-user-instruction agent-instance (preproc-messages (:Messages sess))) sess]))

(defn- maybe-agent-to-spec [obj]
  (if (inference-agent? obj)
    (let [[msgs chat-session] (fetch-messages obj)]
      [{:messages msgs} obj chat-session])
    (if-let [agent (:agent obj)]
      (if (inference-agent? agent)
        (let [[msgs chat-session] (fetch-messages agent)]
          [(assoc (dissoc obj :agent) :messages msgs) agent chat-session])
        [obj nil nil])
      [obj nil nil])))

(defn make-completion [agent-spec]
  (let [[spec agent-inst chat-session] (maybe-agent-to-spec agent-spec)
        msgs (:messages spec)
        result (make-provider-request p/make-completion spec)]
    (when (and chat-session (seq msgs))
      (model/update-agent-chat-session
       chat-session
       (vec (concat msgs [{:role :assistant :content (first result)}]))))
    result))

(def get-embedding (comp first make-embedding))
(def get-completion (comp first make-completion))
