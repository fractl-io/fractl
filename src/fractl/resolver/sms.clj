(ns fractl.resolver.sms
  (:require [fractl.resolver.core :as r]
            [fractl.sms :as sms]
            [fractl.util :as u]))

(defn- sms-eval [inst]
  (let [body (:Body inst)
        from (str (u/getenv "TWILIO_PHONE_NUMBER"))
        to (:To inst)]
    (sms/send-sms {:From from
                   :To to
                   :Body body})))

(def ^:private resolver-fns
  {:eval {:handler sms-eval}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
