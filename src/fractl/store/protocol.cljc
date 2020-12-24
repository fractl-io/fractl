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
  (create-schema [store component-name]
    "Initialize the schema to store entity-instances defined
     in the component. On success, return component-name. If the schema
     already exists, return nil. On failure, raise an exception.")
  (drop-schema [store component-name]
    "Drop the schema for the component. Return component-name on success, nil if the
     schema does not exist. On failure, raise an exception.")
  (upsert-instance [store entity-name instance]
    "Insert or update the instance in the store. On success, return instance.
     On failure, raise an exception.")
  (delete-by-id [store entity-name id]
    "Delete the instance with the given id. On success, return id.
     If the instance does not exist, return nil. On failure, raise an exception.")
  (query-by-id [store entity-name query-sql ids]
    "Return the instances with the given :Id attributes. Return nil if the instances
     does not exist. On failure, raise an exception.")
  (do-query [store query query-params]
    "Return all instances that satisfy the query. Return nil if no data found.
     On failure or if the query is not supported, raise an exception.")
  (compile-query [store query-pattern]
    "Compile the query from a dataflow pattern to a format understood by this
     store implementation."))
