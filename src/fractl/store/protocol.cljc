(ns fractl.store.protocol)

(defprotocol Store
  "The interface for all storage layer technologies."
  (open-connection [store connection-info]
    "Open a connection to the storage layer, if not already connected.
     Return true on success. Raise an exception if a new
     connection cannot be obtained.")
  (close-connection [store]
    "Close the active connection, return true on success, false if the connection
     cannot be closed.")
  (create-schema [store model-name]
    "Initialize the schema to store entity-instances defined
     in the model. On success, return model-name. If the schema
     already exists, return nil. On failure, raise an exception.")
  (drop-schema [store model-name]
    "Drop the schema for the model. Return model-name on success, nil if the
     schema does not exist. On failure, raise an exception.")
  (upsert-instance [store entity-name instance]
    "Insert or update the instance in the store. On success, return instance.
     On failure, raise an exception.")
  (delete-instance [store entity-name instance]
    "Delete the instance, the only attribute required to be present is :Id.
     On success, return instance. If the instance does not exist, return nil.
     On failure, raise an exception.")
  (find-by-id [store entity-name id]
    "Return the instance with the given :Id attribute. Return nil if the instance
     does not exist. On failure, raise an exception.")
  (find-by-query [store query] ;; TODO: define query format, maybe reuse honeysql.
    "Return all instances that satisfy the query. Return nil if no data found.
     On failure or if the query is not supported, raise an exception."))
