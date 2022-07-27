(ns fractl.resolver.sms
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
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

(defmake :sms
  (fn [resolver-name config]
    (r/make-resolver resolver-name resolver-fns)))
