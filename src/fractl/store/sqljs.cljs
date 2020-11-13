(ns fractl.store.sqljs
  "The storage layer implementation for SQL.js database."
  (:require [fractl.store.sqljs-protocol :as p]
            [fractl.store.sqljs-internal :as i]))

(defn make []
  (reify p/sql-store
    (create-schema [model-name]
      (i/create-schema model-name))
    (drop-schema [model-name]
      (i/drop-schema model-name))))