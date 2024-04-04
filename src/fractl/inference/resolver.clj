(ns fractl.inference.resolver
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.inference.langchain :as lchain]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(def ^:private llm-handles (atom {}))

(defn- clean-up-attributes [instance]
  (let [attrs (cn/instance-attributes instance)]
    (dissoc attrs :Id li/id-attr li/path-attr)))

(defn- maybe-assoc-openai-defaults [inst]
  (if (:apiKey inst)
    inst
    (assoc inst :apiKey (u/getenv "OPENAI_API_KEY"))))

(defn- llm-create [_ instance]
  (case (cn/instance-type-kw instance)
    :Fractl.Llm.Core/OpenAiChatModel
    (let [instance (maybe-assoc-openai-defaults instance)
          h (lchain/make-openai-chatmodel (clean-up-attributes instance))]
      (swap! llm-handles assoc (:Id instance) h)
      instance)
    (u/throw-ex (str "unsupported llm provider: " (cn/instance-type-kw instance)))))

(defn- preproc-message [msg]
  (let [t (cn/instance-type-kw msg)
        cont (:Content msg)]
    (case t
      :Fractl.Llm.Core/UserMessage
      [:user cont]
      :Fractl.Llm.Core/AiMessage
      [:ai cont]
      (u/throw-ex (str "invalid message type: " t)))))

(defn- llm-eval [_ event-instance]
  (let [model (:Model event-instance)
        model-type (cn/instance-type-kw model)
        result
        (case model-type
          :Fractl.Llm.Core/OpenAiChatModel
          (lchain/openai-generate
           (get @llm-handles (:Id model))
           (mapv preproc-message (:Messages event-instance)))
          (u/throw-ex (str "invalid llm provider: " model-type)))]
    (cn/make-instance
     :Fractl.Llm.Core/AiMessage
     {:Text result})))

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
