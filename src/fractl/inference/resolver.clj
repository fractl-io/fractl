(ns fractl.inference.resolver
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.inference.langchain :as lchain]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(defn- clean-up-attributes [instance]
  (let [attrs (cn/instance-attributes instance)]
    (dissoc attrs :Id li/id-attr li/path-attr)))

(defn- llm-create [_ instance]
  (case (cn/instance-type-kw instance)
    :Fractl.Llm.Core/OpenAiChatModel
    (lchain/make-openai-chatmodel (clean-up-attributes instance))
    (u/throw-ex (str "unsupported inference provider: " (cn/instance-type-kw instance)))))

(defn- llm-eval [_ event-instance]
  event-instance)

(defn- register-resolver [config entity-names]
  (let [k :fractl-inference]
    (rg/register-resolver-type
     k (fn [_ _]
         (r/make-resolver
          k {:create (partial llm-create config)
             :eval (partial llm-eval config)})))
    (rg/register-resolver {:name k :type k :paths entity-names})))

(defn register [config]
  (if-let [ents (seq (cn/entity-names :Fractl.Llm.Core))]
    (register-resolver config ents)
    (u/throw-ex "Fractl.Llm model not initialized.")))
