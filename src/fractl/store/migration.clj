(ns fractl.store.migration
  (:require [fractl.store.protocol :as p])
  (:import [liquibase Contexts Liquibase]
           [liquibase.database Database DatabaseFactory]
           [liquibase.database.jvm JdbcConnection]
           [liquibase.exception LiquibaseException]
           [liquibase.resource FileSystemResourceAccessor]
           [java.io File]
           [java.sql Connection DriverManager SQLException]))

(defn lqb-init [store conf]
  (let [{db-url :url username :username password :password}
        (p/parse-connection-info store conf)
        conn #(DriverManager/getConnection db-url username password)]
    {:connection conn}))

(defn lqb-migrate [lqb]
  (let [^Connection connection ((:connection lqb))]
    (try
      (let [^Database database (.findCorrectDatabaseImplementation
                                (DatabaseFactory/getInstance)
                                (JdbcConnection. connection))
            ^File base-dir (File. ".")
            ^Liquibase liquibase (Liquibase. "db/migration/changelog.sql"
                                             (FileSystemResourceAccessor. (into-array File [base-dir]))
                                             database)]
        (.update liquibase (Contexts.)))
      (catch Exception ex
        (when connection
          (.rollback connection))
        (throw ex))
      (finally
        (when connection
          (.close connection))))))

(def init lqb-init)
(def migrate lqb-migrate)
