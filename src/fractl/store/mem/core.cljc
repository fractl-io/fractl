(ns fractl.store.mem.core
  (:require [fractl.store.mem.internal :as i]
            [fractl.store.protocol :as p]
            [fractl.util :as u]))

(defn- make-internal []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (u/safe-set
         datasource
         connection-info)
        true)
      (close-connection [_]
        true)
      (parse-connection-info [_ connection-info]
        connection-info)
      (connection-info [_]
        (if @datasource
          @datasource
          {}))
      (create-schema [_ component-name])
      (drop-schema [_ component-name])
      (update-instance [_ entity-name instance]
        ;; always over-write
        (i/upsert-instance entity-name instance))
      (load-component [_ component-name]
        (u/throw-ex "Not implemented"))
      (upsert-instance [_ entity-name instance]
        (i/upsert-instance entity-name instance))
      (delete-by-id [_ entity-name id-attr-name id]
        (i/delete-by-id entity-name id-attr-name id))
      (delete-all [_ entity-name _]
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
      (call-in-transaction [_ f]
        (f nil))
      (get-reference
        [_ path refs]
        (i/get-reference path refs)))))

(defn make
  []
  (make-internal))

#?(:cljs
   (defn reagent-make
     []
     (make-internal)))
