(ns fractl.store.migration
  (:require [clojure.set :as set]
            [fractl.lang.raw :as raw]
            [fractl.lang.internal :as li]
            [fractl.store.protocol :as p])
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

(defn- entity-def? [obj]
  (let [n (first obj)]
    (or (= n 'entity)
        (and (= n 'relationship)
             (let [s (second obj)
                   scm (if (map? s)
                         (li/record-attributes s)
                         (nth obj 2))]
               (:between (:meta scm)))))))

(defn- extract-entity-names [defs]
  (set (mapv #(li/record-name (second %)) (filter entity-def? defs))))

(defn- make-alter-diff [entities new-def old-def]
  ;; TODO: compare the chnages in the new and old defs, generate abstract alter-commands
  ;; TODO: also consider contains-relationships
  )

(defn diff [component old-def]
  (let [new-def (rest (raw/as-edn component))
        new-entities (extract-entity-names new-def)
        old-entities (extract-entity-names old-def)
        active-entities (set/intersection old-entities new-entities)]
    {:drop (seq (set/difference old-entities new-entities))
     :alter (make-alter-diff active-entities new-def old-def)}))
