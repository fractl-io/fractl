(ns fractl.store.reagent.core
  (:require [fractl.store.reagent.internal :as i]
            [fractl.store.protocol :as p]
            [fractl.util :as u]))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (u/safe-set
         datasource
         connection-info)
        true)
      (close-connection [_]
        true)
      (connection-info [_]
        (if @datasource
          @datasource
          {}))
      (create-schema [_ component-name])
      (drop-schema [_ component-name])
      (update-instance [_ entity-name instance]
        ;; always over-write
        (i/upsert-instance entity-name instance))
      (upsert-instance [_ entity-name instance]
        (i/upsert-instance entity-name instance))
      (delete-by-id [_ entity-name id]
        (i/delete-by-id entity-name id))
      (delete-all [_ entity-name]
        (i/delete-all entity-name))
      (query-by-id [_ entity-name query ids]
        (i/query-by-id entity-name ids))
      (query-by-unique-keys [_ entity-name unique-keys unique-values]
        ;; no unique-key enforcement, always over-write
        nil)
      (do-query [_ query params]
        (u/throw-ex "Not implemented"))
      (query-all [_ entity-name query]
        (i/query-all entity-name query))
      (compile-query
        [_ query-pattern]
        (i/compile-to-indexed-query query-pattern))
      (get-reference
        [_ path refs]
        (i/get-reference path refs)))))

(def state i/inst-store)
