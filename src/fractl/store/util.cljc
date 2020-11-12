(ns fractl.store.util
  (:require [cheshire.core :as json]
            [fractl.component :as cn]))

(defn normalize-connection-info [connection-info]
  (if-let [f (:decrypt-fn connection-info)]
    (let [pswd (f (:password connection-info))]
      (assoc connection-info :password pswd))
    connection-info))

(defn result-as-instance [entity-name id-key json-key result]
  (let [^org.h2.jdbc.JdbcClob clob (json-key result)
        cloblen (.length clob)
        ^java.util.UUID id (id-key result)
        parsed-obj (assoc (json/parse-string (.getSubString clob 1 cloblen) true)
                          :Id (.toString id))]
    (cn/make-instance entity-name parsed-obj)))

(defn results-as-instances [entity-name id-key json-key results]
  (doall (map (partial result-as-instance entity-name id-key json-key) results)))

(defn make-result-keys [entity-name]
  (let [n (name (second entity-name))]
    [(keyword (str n "/ID")) (keyword (str n "/INSTANCE_JSON"))]))
