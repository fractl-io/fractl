(ns fractl.resolver
  (:require [fractl.resolver.registry :as rg]))

(def resolver-for-path rg/resolver-for-path)

(def override-resolver rg/override-resolver)
(def compose-resolver rg/compose-resolver)
(def composed? rg/composed?)
(def override? rg/override?)

(defn call-resolver-upsert [resolver inst]
  ((rg/resolver-upsert resolver) inst))

(defn call-resolver-delete [resolver inst]
  ((rg/resolver-delete resolver) inst))

(defn call-resolver-get [resolver inst]
  ((rg/resolver-get resolver) inst))

(defn call-resolver-query [resolver query]
  ((rg/resolver-query resolver) query))

(defn call-resolver-eval [resolver event-inst]
  ((rg/resolver-eval resolver) event-inst))
