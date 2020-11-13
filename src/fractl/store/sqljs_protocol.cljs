(ns fractl.store.sqljs-protocol)

(defprotocol sql-store
  (create-schema [store model-name])
  (drop-schema [store model-name]))
