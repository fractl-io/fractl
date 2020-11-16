(ns fractl.store.sqljs-internal
  (:require [clojure.string :as string]
            ["sql.js" :default initSqlJs]
            [fractl.store.db-internal :as dbi]
            [fractl.util :as u]
            [fractl.component :as cn]))

;; Store for databases.
(defonce sql-db (atom nil))

(def db-new (atom {}))

(defn new-db [create-statement]
  (let [db-ref (atom {})
        _ (-> (initSqlJs)
              (.then (fn [sql]
                       (let [_  (js/d ::sql sql)
                             db (sql.Database.)]
                         (js/d ::db db)
                         (-> db
                             (.run create-statement))
                         (reset! db-new db))))
              (.catch (fn [err]
                        (print "error : " err))))]
    db-ref))

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

;; Clean this up in a better approach.
(defn create-sqlite-conn
  ([table table-name attr]
   (-> (initSqlJs)
       (.then (fn [sql]
                (let [_ (js/d ::sql sql)
                      db (sql.Database.)]
                  (js/d ::db db)
                  (-> db
                      (.then (if (.exec [table])
                         table-name
                         (u/throw-ex (str "Failed to create index table for identity column - "
                                          [table-name attr]))))
                      (.catch (fn [err]
                                (print "no db connection. more: " err))))
                  (reset! db-new db))))
       (.catch (fn [err]
                 (print "error: " err)))))
  ([table table-name]
   (-> (initSqlJs)
       (.then (fn [sql]
                (let [_ (js/d ::sql sql)
                      db (sql.Database.)]
                  (js/d ::db db)
                  (-> db
                      (.then (if (.exec [table])
                               table-name
                               (u/throw-ex (str "Failed to create index table for identity column - "
                                                [table-name]))))
                      (.catch (fn [err]
                                (print "no db connection. more: " err))))
                  (reset! db-new db))))
       (.catch (fn [err]
                 (print "error: " err)))))
  ([table]
   (-> (initSqlJs)
       (.then (fn [sql]
                (let [_ (js/d ::sql sql)
                      db (sql.Database.)]
                  (js/d ::db db)
                  (if (.exec db [table])
                    (reset! sql-db db))
                  (-> db
                      (.exec [table]))
                  (reset! db-new db))))
       (.catch (fn [err]
                 (print "error: " err))))))

(defn prepare-sqlite
  [sql-string]
  (let [db-ref (atom {})
        _ (-> (initSqlJs)
              (.then (fn [sql]
                       (let [_ (js/d ::sql sql)
                             db (sql.Database.)]
                         (js/d ::db db)
                         (-> db
                             (.prepare sql-string))
                         (reset! db-new db))))
              (.catch (fn [err]
                        (print "error: " err))))]
    db-ref))

(defn runner-sqlite
  "Run on prepared sqlite statements."
  [prep attrs]
  (-> (initSqlJs)
      (.then (fn [sql]
               (.run @prep [attrs])))
      (.catch (fn [err]
                (print "error: " err)))))

(defn- create-identity-index! [entity-table-name ident-attr]
  (let [table (create-identity-index-sql entity-table-name (dbi/db-ident ident-attr))]
    ; (fn []
    ;   [:div "db start"])
    (create-sqlite-conn table entity-table-name ident-attr)))


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
  (let [table (create-entity-table-sql tabname ident-attr)]
    (create-sqlite-conn table tabname)
    ; (fn []
    ;   [:div "db entity-table"])
    ))

(defn- create-index-table! [entity-schema entity-table-name attrname idxattr]
  (let [[tabsql idxsql] (create-index-table-sql
                         entity-table-name attrname
                         (cn/unique-attribute? entity-schema idxattr))]
    (when-not (and (create-sqlite-conn tabsql)
                   (create-sqlite-conn idxsql))
      (u/throw-ex (str "Failed to create lookup table for " [entity-table-name attrname])))))

(defn- create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [entity-schema entity-table-name ident-attr indexed-attrs]
  (create-entity-table! entity-table-name ident-attr)
  (when ident-attr
    (create-identity-index! entity-table-name ident-attr))
  (let [cit (partial create-index-table! entity-schema entity-table-name)]
    (doseq [idxattr indexed-attrs]
      (let [attrname (dbi/db-ident idxattr)]
        (cit attrname idxattr)
        (when-not (create-sqlite-conn [(dbi/create-index-sql entity-table-name attrname nil)])
          (u/throw-ex (str "Failed to create index for " [entity-table-name attrname])))))
    entity-table-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [db-schema-name]
  (if (seq (create-sqlite-conn [(dbi/create-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [db-schema-name]
  (if (seq (create-sqlite-conn [(dbi/drop-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the model.
   No needed to create extra connection unlike h2, since, it's in-memory."
  [model-name]
  (let [scmname (dbi/db-schema-for-model model-name)]
    (create-db-schema! scmname)
    (doseq [ename (cn/entity-names model-name)]
      (let [tabname (dbi/table-for-entity ename)
            schema (cn/entity-schema ename)
            indexed-attrs (dbi/find-indexed-attributes ename schema)]
        (create-tables! schema tabname :Id indexed-attrs)))
    model-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [model-name]
  (let [scmname (dbi/db-schema-for-model model-name)]
    (drop-db-schema! scmname)
    model-name))

(defn- upsert-index-statement [table-name id attrval]
  (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
        pstmt (prepare-sqlite [sql])]
    (runner-sqlite pstmt [id attrval])
    pstmt))

(defn- upsert-indices!
  "Insert or update new index entries relevant for an entity instance.
  The index values are available in the `attrs` parameter."
  [entity-table-name indexed-attrs id]
  (let [index-tabnames (dbi/index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [pstmt (upsert-index-statement tabname
                                          id (attrname indexed-attrs))]
        (create-sqlite-conn pstmt)))))

(defn- upsert-inst-statement [table-name id obj]
  (let [sql (str "MERGE INTO " table-name " KEY (ID) VALUES (?, ?)")
        pstmt (prepare-sqlite [sql])]
    (runner-sqlite pstmt [id obj])
    pstmt))

(defn- upsert-inst!
  "Insert or update an entity instance."
  [table-name inst]
  (let [attrs (cn/serializable-attributes inst)
        id (:Id attrs)
        obj (.stringify js/JSON (clj->js (dissoc attrs :Id)))
        pstmt (upsert-inst-statement table-name id obj)]
    (create-sqlite-conn pstmt)))

(defn upsert-instance [entity-name instance]
  (let [tabname (dbi/table-for-entity entity-name)
        indexed-attrs (dbi/find-indexed-attributes entity-name)]
    (upsert-inst! tabname instance)
    (upsert-indices! tabname indexed-attrs (:Id instance))
    instance))

(defn- delete-index-statement [table-name colname id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        pstmt (prepare-sqlite [sql])]
    (runner-sqlite pstmt [id])
    pstmt))

(defn- delete-indices!
  "Delete index entries relevant for an entity instance."
  [entity-table-name indexed-attrs id]
  (let [index-tabnames (dbi/index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [pstmt (delete-index-statement
                   tabname
                   (dbi/db-ident attrname) id)]
        (create-sqlite-conn pstmt)))))

(defn- delete-inst-statement [table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        pstmt (prepare-sqlite [sql])]
    (runner-sqlite pstmt [id])
    pstmt))

(defn- delete-inst!
  "Delete an entity instance."
  [tabname id]
  (let [pstmt (delete-inst-statement tabname id)]
    (create-sqlite-conn pstmt)))

(defn delete-instance [entity-name instance]
  (let [id (:Id instance)
        tabname (dbi/table-for-entity entity-name)
        indexed-attrs (dbi/find-indexed-attributes entity-name)]
    (delete-indices! tabname indexed-attrs id)
    (delete-inst! tabname id)
    id))

(defn- find-by-id-statement [entity-table-name id]
  (let [sql (str "SELECT * FROM " entity-table-name " WHERE id = ?")
        pstmt (prepare-sqlite [sql])]
    (runner-sqlite pstmt [id])
    pstmt))

(defn find-by-id [entity-name id]
  (let [tabname (dbi/table-for-entity entity-name)
        pstmt (find-by-id-statement tabname id)]
    (-> (initSqlJs)
        (.then (fn [sql]
                 (let [db (sql.Database.)]
                   (.run db pstmt))))
        (.catch (fn [err]
                  (print "error: " err))))))
