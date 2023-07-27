(ns fractl.store.migration
  (:import [org.flywaydb.core Flyway]))

(defn migrate [db-url username password]
  (let [^Flyway flyway (.load (.dataSource (Flyway/configure) db-url username password))]
    (.migrate flyway)))
