(ns fractl.store.migration
  (:require [fractl.store.protocol :as p])
  (:import [org.flywaydb.core Flyway]
           [org.flywaydb.core.api.configuration FluentConfiguration]))

(defn init [store conf]
  (let [{db-url :url username :username password :password}
        (p/parse-connection-info store conf)
        ^FluentConfiguration conf (Flyway/configure)]
    (.locations conf (into-array String ["classpath:db/migration"]))
    (.dataSource conf db-url username password)
    (.load conf)))

(defn migrate [^Flyway handle]
  (.migrate handle))

(defn baseline [^Flyway handle]
  (.baseline handle))
