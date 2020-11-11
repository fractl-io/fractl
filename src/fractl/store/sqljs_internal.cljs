(ns fractl.store.sqljs-internal
  (:require [clojure.string :as string]
            ["sql.js" :default initSqlJs]
            [fractl.store.db-internal :as dbi]
            [fractl.util :as u]))

;; Store for databases.
(defonce sql-db (atom nil))

(defn- create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  (str dbi/create-table-prefix " " tabname " "
       (if ident-attr
         (str "(" (dbi/db-ident ident-attr) " UUID, ")
         "(")
       "instance_json CLOB)"))

(defn- create-index-table-sql
  "Given a database-type, entity-table-name and attribute-column name, return the
  DML statements for creating an index table and the index for its 'id' column."
  [entity-table-name colname unique?]
  (let [index-tabname (dbi/index-table-name entity-table-name colname)]
    [(str dbi/create-table-prefix " " index-tabname " "
          ;; `id` is not a foreign key reference to the main table,
          ;; because insert is fully controlled by the fractl runtime and
          ;; we get an index for free.
          "(id UUID, "
          ;; Storage and search can be optimized by inferring a more appropriate
          ;; SQL type for `colname`, see the issue https://ventur8.atlassian.net/browse/V8DML-117.
          colname " CLOB"
          (if unique? (str ",UNIQUE(" colname "))") ")"))
     (dbi/create-index-sql index-tabname "id" true)]))

(defn- create-identity-index-sql [entity-table-name colname]
  (str dbi/create-unique-index-prefix
       " " (dbi/index-name entity-table-name)
       " ON " entity-table-name "(" colname ")"))

;; Ugly function replace with better approach.
(defn- create-identity-index! [entity-table-name ident-attr]
  (let [table (create-identity-index-sql entity-table-name (dbi/db-ident ident-attr))
        _ (-> (initSqlJs)
              (.then (fn [sql]
                       (let [db (sql.Database.)]
                         (if (.exec db [table])
                           (do
                             entity-table-name
                             (reset! sql-db db))
                           (u/throw-ex (str "Failed to create index table for identity column - "
                                            [entity-table-name ident-attr]))))))
              (.catch (fn [err]
                        (print "error: " err))))]
    (fn []
      [:div "db start"])))


(comment
  (defn sqlite []
          (let [_ (-> (initSqlJs)
                      (.then (fn [sql]
                               (let [db (sql.Database.)
                                     table "CREATE TABLE hello (a int, b char);
                INSERT INTO hello VALUES (0, 'hello');
                INSERT INTO hello VALUES (1, 'world');"]
                                 (print (.run db table))
                                 (-> db
                                     (.exec "select * from hello")
                                     ;; (js/console.log)
                                     )
                                 (reset! sql-db db)
                                 )))
                      (.catch (fn [err]
                                (print "error : " err))))]
            (fn []
              [:div "db start"])
            ))

        (sqlite))

#_(defn sqlite []
  (let [_ (-> (initSqlJs)
              (.then (fn [sql]
                       (reset! sql-generator sql)
                       (let [_  (js/console.log "sql : " sql)
                             db (sql.Database.)]
                         (.run db "CREATE TABLE hello (a int, b char);")
                         (.exec db "SELECT * FROM hello")
                         (print "db : "db)
                         (reset! sql-db db))))
              (.catch (fn [err]
                        (print "error : " err))))]))

(defn- create-entity-table!
  "Create a table to store instances of an entity. As 'identity-attribute' is
  specified to be used as the primary-key in the table."
  [tabname ident-attr]
  (let [table (create-entity-table-sql tabname ident-attr)
        _ (-> (initSqlJs)
              (.then (fn [sql]
                       (let [db (sql.Database.)]
                         (if (.exec db [table])
                           (do
                             tabname
                             (reset! sql-db db))
                           (u/throw-ex (str "Failed to create table for " tabname))))))
              (.catch (fn [err]
                        (print "error: " err))))]
    (fn []
      [:div "db entity-table"])))
