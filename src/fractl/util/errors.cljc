(ns fractl.util.errors
  "A namespace dedicated to handling HTTP errors within the Fractl system.
  It includes definitions of various error types, their corresponding error messages,
  and functionality to manage client-facing error messages. This setup allows for
  consistent error handling across the system, with the ability to override error messages
  for client-side presentation while maintaining detailed internal error logs."
  (:require[fractl.util :as u]))

;; currently, we use two functions to handle errors: throw-error, internal-error
;; throw-error raises an exception that is later caught and message flows to internal-error, and it is logged as well as sent to client
;; using methods below we can choose to modify client-side error messages or return a generic response
(defn default-client-error []
  "Generic client error message, displayed when client error disabled"
  "Internal error on server")

(def disabled-client-errors
  "Symbols of errors for which generic client message should be sent (for example, in case error message may reveal
  sensitive BE info)"
  #{})

;; Functions for each error type thrown using throw-error. Name format "error-key-error"
(defn- invalid-attribute-error [recname ks]
  (str recname " - invalid attribute(s) found - " ks))

(defn- client-invalid-attribute-error [recname ks]
  "Overrides client side message for invalid-attribute error"
  (str "Invalid attribute(s) found - " ks))

(defn- check-failed-error [aval aname]
  (str "Check failed, invalid value " aval " for " aname))

(defn- format-mismatch-error [aname]
  (str "Format mismatch - " aname))

(defn- type-mismatch-error [attr-name type-name]
  (str "Expected type for " attr-name " is " type-name))

(defn- invalid-list-element-error [aname]
  (str "Invalid list element for " aname))

(defn- not-a-set-error [aname]
  (str "Not a set - " aname))

(defn- invalid-set-element-error [aname]
  (str "Invalid set element for " aname))

(defn- no-default-value-error [aname]
  (str "No default value defined for " aname))

(defn- attribute-type-mismatch-error [attrname recname]
  (str "Attribute " attrname " is not of type " recname))

(defn- no-record-set-error [attrname]
  (str "No record set for attribute " attrname))

(defn- attribute-not-in-schema-error [attr-name]
  (str "Attribute not in schema - " attr-name))

(defn- schema-not-found-error [recname]
  (str "Schema not found for " recname))

(defn- invalid-operator-error [cond-expr]
  (str "Invalid condition in event pattern - " cond-expr))

(def error-functions
  "map of error keys to their corresponding error functions"
  {:invalid-attribute invalid-attribute-error
   :check-failed check-failed-error
   :format-mismatch format-mismatch-error
   :type-mismatch type-mismatch-error
   :invalid-list-element invalid-list-element-error
   :not-a-set not-a-set-error
   :invalid-set-element invalid-set-element-error
   :no-default-value no-default-value-error
   :attribute-type-mismatch attribute-type-mismatch-error
   :no-record-set no-record-set-error
   :schema-not-found schema-not-found-error
   :attribute-not-in-schema attribute-not-in-schema-error
   :invalid-operator invalid-operator-error})

(def client-error-functions
  {
   ;; Add other overrides as necessary
   })

(defn get-error-messages [error-key args]
  "Returns internal and client side error messages for given error key
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

;; for some errors we directly call internal-error and supply it with a string
;; below is consolidation of such errors
(def internal-error-messages
  {:query-failure "Failed to process query request - %s"
   :auth-disabled "cannot process %s - authentication not enabled"})

(defn get-internal-error-message [key & args]
  (format (get internal-error-messages key) args))

(defn make-error
  "Return an instance of the error record with the given message
   and additional attributes."
  ([msg attributes]
   (u/make-record-instance :record :error (assoc attributes :message msg)))
  ([msg]
   (make-error msg nil)))

(defn throw-ex-info
  "Throws an exception with the given error object."
  ([msg error-obj]
   (throw (ex-info msg error-obj)))
  ([msg]
   (throw-ex-info msg nil)))

(defn extract-client-message-from-ex [ex-obj]
  "Extracts client message from the additional data map of exception."
  (let [ex-data-map (ex-data ex-obj)
        client-msg (:client ex-data-map)]
    client-msg))

(defn- throw-error
  "Create a structured error object and throw it as an exception."
  ([msgs attributes]
   (let [error-msg (str "component/error: " (:internal msgs))
         error-details {:error (make-error (:internal msgs) attributes)
                        :client (:client msgs)
                        :type "component/error"}]
     (throw-ex-info error-msg error-details)))
  ([msgs]
   (throw-error msgs {})))

(defn raise-error
  "Returns internal and client side error messages for given error key"
  [error-key args]
 (throw-error (get-error-messages error-key args)))
