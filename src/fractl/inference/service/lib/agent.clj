(ns fractl.inference.service.lib.agent
  (:require [clojure.set :as set]
            [fractl.inference.provider.openai :as p]
            [fractl.inference.service.lib.compose :as compose]
            [fractl.inference.service.lib.prompt :as prompt]
            [fractl.inference.service.lib.output-parser :as output]
            [fractl.inference.service.lib.retriever :as retriever]
            [fractl.inference.service.lib.agent-planner :as planner]
            [fractl.inference.util :as util]))

(def args-validator-make-docs-agent
  (util/make-validator-explainer
    [:map
     [:user-question :string]
     [:question-context :map]
     [:app-uuid :string]]))

(defn make-docs-rag-agent [options]
  (compose/chain {:chain-name "DOCS-AGENT"}
                 args-validator-make-docs-agent
                 (fn [m] (set/rename-keys m {:user-question :text-content}))
                 ;; ---
                 (compose/assok :embedding p/make-openai-embedding) ; needs :text-content
                 (compose/assok :all-tools retriever/retrieve-docs) ; needs :app-uuid and embedding
                 (compose/assok :messages  prompt/make-docs-rag-messages)
                 (compose/assok :answer-text p/make-openai-completion)
                 (fn [m] (select-keys m [:answer-text]))))

(def args-validator-make-planner-agent
  (util/make-validator-explainer
    [:map
     [:user-question :string]
     [:background    :map]
     [:use-docs?     :boolean]
     [:app-uuid      :string]]))

(defn make-planner-agent
  [options]
  ;; TODO: output-checking
  ;; TODO: evaluation
  ;; TODO: retries (with max-retries option)
  (let [{:keys [max-retries]
         :or {max-retries 2}} options
        planner-core (compose/chain {:chain-name "PLANNER-AGENT-CORE"
                                     :max-retries max-retries}
                                    (compose/assok :answer-text p/make-openai-completion)
                                    (compose/assok :plantext (compose/applyk output/llm-plan-parser :answer-text))
                                    (let [f (compose/applyk planner/compose-datafow :plantext :all-tools)]
                                      (fn [m] (try
                                                (assoc m :patterns (f m))
                                                (catch Exception e
                                                  (.printStackTrace e)
                                                  (assoc m :errormsg m))))))]
    (compose/chain {:chain-name "PLANNER-AGENT"}
                   args-validator-make-planner-agent ; validate args
                   ;;-- classification
                   (fn [m] (assoc m :question (:user-question m)))
                   (compose/assok :messages prompt/make-classify-intent-messages)
                   (compose/assok :analysis-text p/make-openai-completion)
                   ;;-- planner
                   (fn [m] (set/rename-keys m {:user-question :text-content})) ; next step needs :text-content
                   (compose/assok :embedding p/make-openai-embedding) ; needs :text-content
                   (compose/assok :all-tools retriever/retrieve-tools) ; needs :app-uuid and embedding
                   (if (:use-docs? options)
                     (compose/assok :all-docs retriever/retrieve-docs)
                     identity)
                   (compose/assok :messages prompt/make-planner-messages)
                   ;; retry-enabled
                   planner-core
                   (fn [m] (select-keys m [:answer-text :patterns])))))
