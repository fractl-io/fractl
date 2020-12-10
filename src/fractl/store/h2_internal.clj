(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]
            [fractl.store.util :as su]
            [fractl.store.sql :as sql]
            [fractl.store.db-internal :as dbi])
  (:import [java.sql PreparedStatement]))

(defn create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  [(str dbi/create-table-prefix " " tabname " "
        (if ident-attr
          (str "(" (dbi/db-ident ident-attr) " UUID, ")
          "(")
        "instance_json JSON)")])

(defn create-index-table-sql
  "Given a database-type, entity-table-name and attribute-column name, return the
  DML statements for creating an index table and the index for its 'id' column."
  [entity-table-name colname coltype unique?]
  (let [index-tabname (dbi/index-table-name entity-table-name colname)]
    [[(str dbi/create-table-prefix " " index-tabname " "
          ;; `id` is not a foreign key reference to the main table,
          ;; because insert is fully controlled by the V8 runtime and
          ;; we get an index for free.
           "(id UUID, "
          ;; Storage and search can be optimized by inferring a more appropriate
          ;; SQL type for `colname`, see the issue https://ventur8.atlassian.net/browse/V8DML-117.
           colname " " coltype
           (if unique? (str ",UNIQUE(" colname "))") ")"))]
     [(dbi/create-index-sql index-tabname colname unique?)]]))

(defn create-identity-index-sql [entity-table-name colname]
  [(str dbi/create-unique-index-prefix
        " " (dbi/index-name entity-table-name)
        " ON " entity-table-name "(" colname ")")])

(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "INSERT INTO " table-name " VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [(jdbcp/set-parameters pstmt [id attrval]) nil]))

(defn upsert-inst-statement [conn table-name id obj]
  (let [sql (str "MERGE INTO " table-name " KEY (ID) VALUES (?, ? FORMAT JSON)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [(jdbcp/set-parameters pstmt [id obj]) nil]))

(defn validate-ref-statement [conn index-tabname colname ref]
  (let [sql (str "SELECT 1 FROM " index-tabname " WHERE " colname " = ?")
        ^PreparedStatement pstmt (jdbcp/set-parameters
                                  (jdbc/prepare conn [sql])
                                  [ref])]
    [pstmt nil]))

(defn delete-index-statement [conn table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id])
    [pstmt nil]))

(defn delete-inst-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id])
    [pstmt nil]))

(defn- query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setString pstmt 1 (str id))
    pstmt))

(defn query-by-id [datasource entity-name query-sql ids]
  (with-open [conn (jdbc/get-connection datasource)]
    (let [[id-key json-key] (su/make-result-keys entity-name)]
      ((partial su/results-as-instances entity-name id-key json-key)
       (flatten (map #(let [pstmt (query-by-id-statement conn query-sql %)]
                        (jdbc/execute! pstmt))
                     (set ids)))))))

(defn do-query [datasource query-sql query-params]
  (with-open [conn (jdbc/get-connection datasource)]
    (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
      (jdbcp/set-parameters pstmt query-params)
      (jdbc/execute! pstmt))))

(def compile-to-indexed-query (partial sql/compile-to-indexed-query
                                       dbi/table-for-entity
                                       dbi/index-table-name))

(defn transact! [datasource f]
  (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (f txn))))

(defn execute-sql! [conn sql]
  (jdbc/execute! conn sql))

(defn execute-stmt! [_ stmt _]
  (jdbc/execute! stmt))