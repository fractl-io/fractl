(ns fractl.inference.service.core
  (:require fractl.inference.service.model ; force-load component
            [fractl.inference.service.resolver :as api-resolver]))

(defn init [] (api-resolver/register-resolver))
