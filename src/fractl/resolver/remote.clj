(ns fractl.resolver.remote
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(defn remote-upsert [inst]
  )

(defn remote-delete [inst]
  )

(defn remote-get [inst]
  )

(defn remote-query [query]
  )

(defn remote-eval [event-inst]
  )

(defn make [resolver-name]
  (r/make-resolver resolver-name
                   {:upsert remote-upsert
                    :delete remote-delete
                    :get remote-get
                    :query remote-query
                    :eval remote-eval}))
