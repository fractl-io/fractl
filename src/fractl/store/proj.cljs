(ns fractl.store.proj)

(defprotocol Store
  "The interface for all storage layer technologies."
  #_(open-connection [this]
                     "Open a connection to the storage layer, if not already connected.
                      Return true on success. Raise an exception if a new
                      connection cannot be obtained.")
  #_(close-connection [this]
                      "Close the active connection, return true on success, false if the connection
                       cannot be closed.")
  (create-schema [this component-name]
                 "Initialize the schema to store entity-instances defined
                  in the component. On success, return component-name. If the schema
                  already exists, return nil. On failure, raise an exception."))
