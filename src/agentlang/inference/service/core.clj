(ns agentlang.inference.service.core
  (:require [agentlang.util :as u]
            [agentlang.evaluator :as ev]
            agentlang.inference.service.model ; force-load component
            [agentlang.inference.service.resolver :as api-resolver]
            [agentlang.inference.provider.core :as p]))

(defn init [] (api-resolver/register-resolver))
