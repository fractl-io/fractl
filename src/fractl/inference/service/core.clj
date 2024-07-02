(ns fractl.inference.service.core
  (:require [fractl.util.runtime :as fur]
            fractl.inference.service.model                  ; force-load component
            [fractl.inference.service.resolver :as api-resolver]))

(defn init []
  (fur/set-on-init! #(do (api-resolver/register-resolver))))
