(ns fractl.errors.http
  "A namespace dedicated to handling HTTP errors within the Fractl system.
  It includes definitions of various error types, their corresponding error messages,
  and functionality to manage client-facing error messages. This setup allows for
  consistent error handling across the system, with the ability to override error messages
  for client-side presentation while maintaining detailed internal error logs.")

(defn default-client-error []
  "Generic client error message, displayed when client erro disabled"
  "Internal error on server")

(def disabled-client-errors
  "symbols of errors for which generic client message should be sent (for example, in case error message may reveal
  sensitive BE info)"
  #{})

;; Functions for each error type. Name format "error key-error"
(defn invalid-attribute-error [recname ks]
  (str recname " - invalid attribute(s) found - " ks))

(defn client-invalid-attribute-error [recname ks]
  "overrides client side message for invalid-attribute error"
  (str "Invalid attribute(s) found - " ks))

(defn check-failed-error [aval aname]
  (str "Check failed, invalid value " aval " for " aname))

(defn format-mismatch-error [aname]
  (str "Format mismatch - " aname))

(defn expected-type-error [attr-name type-name]
  (str "Expected type for " attr-name " is " type-name))

(defn invalid-list-error [aname]
  (str "Invalid list for " aname))

(defn not-a-set-error [aname]
  (str "Not a set - " aname))

(defn invalid-set-error [aname]
  (str "Invalid set for " aname))

(defn no-default-value-error [aname]
  (str "No default value defined for " aname))

(defn attribute-type-mismatch-error [attrname recname]
  (str "Attribute " attrname " is not of type " recname))

(defn no-record-set-error [attrname]
  (str "No record set for attribute " attrname))

(defn no-schema-found-error [attr-name]
  (str "No schema found for attribute - " attr-name))

(defn attribute-not-in-schema-error [attr-name]
  (str "Attribute not in schema - " attr-name))

(defn schema-not-found-error [recname]
  (str "Schema not found for " recname))

(defn schema-not-found-for-entity-error [recname]
  (str "Schema not found for entity - " recname))

(defn invalid-condition-error [cond-expr]
  (str "Invalid condition in event pattern - " cond-expr))

(def error-functions
  "map of error keys to their corresponding error functions"
  {:invalid-attribute invalid-attribute-error
   :check-failed check-failed-error
   :format-mismatch format-mismatch-error
   :expected-type expected-type-error
   :invalid-list invalid-list-error
   :not-a-set not-a-set-error
   :invalid-set invalid-set-error
   :no-default-value no-default-value-error
   :attribute-type-mismatch attribute-type-mismatch-error
   :no-record-set no-record-set-error
   :no-schema-found no-schema-found-error
   :attribute-not-in-schema attribute-not-in-schema-error
   :schema-not-found schema-not-found-error
   :schema-not-found-for-entity schema-not-found-for-entity-error
   :invalid-condition invalid-condition-error})

(def client-error-functions
  {:invalid-attribute client-invalid-attribute-error
   ;; Add other overrides as necessary
   })

(defn get-error-messages [error-key args]
  "returns internal and client side error messages for given error key
  Returned string will be used for backend logging as well as to be sent to client-side
  Optionally, we can override client-facing message by introducing a function starting with ¨client-¨ in same namespace"
  (let [internal-msg-fn (get error-functions error-key)
        client-msg-fn (get client-error-functions error-key)]
    (if internal-msg-fn
      (let [internal-msg (apply internal-msg-fn args)
            client-msg (if (and client-msg-fn (not (contains? disabled-client-errors error-key)))
                         (apply client-msg-fn args)
                         internal-msg)]
        {:internal internal-msg
         :client (if (contains? disabled-client-errors error-key)
                   (default-client-error)
                   client-msg)})
      {:internal "Unknown error"
       :client (default-client-error)})))