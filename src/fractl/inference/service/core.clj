(ns fractl.inference.service.core
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]
            fractl.inference.service.model ; force-load component
            [fractl.inference.service.resolver :as api-resolver]
            [fractl.inference.provider.core :as p]))

(defn init [] (api-resolver/register-resolver))

(defn post-question [provider-name question-inst]
  (p/call-with-provider
   provider-name
   #(ev/eval-all-dataflows
     {:Fractl.Inference.Service/Create_Question
      {:Instance question-inst}})))
