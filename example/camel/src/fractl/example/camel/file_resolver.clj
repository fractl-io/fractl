(ns fractl.example.camel.file-resolver
  (:require [selmer.parser :as st]
            [fractl.datafmt.json :as json]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.evaluator.camel :as camel]))

(def ^:private endpoint-templates
  {:create "file:{{directory}}?fileName={{fileName}}"})

(defn- file-create [instance]
  (let [ep (st/render (:create endpoint-templates) instance)]
    (when (camel/exec-route {:endpoint ep
                             :user-arg (json/encode (:content instance))})
      instance)))

(rg/register-resolver-type
 :camel-file
 (fn [_ _]
   (r/make-resolver :camel-file {:create file-create})))
