(ns fractl.store.jdbc-internal
  (:require [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]
            [fractl.util :as u])
  (:import [java.sql PreparedStatement]))

(defn validate-ref-statement [conn index-tabname colname ref]
  (let [sql (str "SELECT 1 FROM " index-tabname " WHERE " colname " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string ref)]]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setString pstmt 1 (str id))
    [pstmt nil]))

(defn delete-index-statement [conn table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id]]))

(defn delete-by-id-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id]]))

(defn do-query-statement
  ([conn query-sql query-params]
   (let [^PreparedStatement pstmt
         (jdbc/prepare
          conn (if (map? query-sql)
                 (:query query-sql)
                 [query-sql]))]
     [pstmt query-params]))
  ([conn query-sql]
   (jdbc/prepare conn [query-sql])))

(defn transact-fn! [datasource f]
  (with-open [conn (jdbc/get-connection datasource)]
    (jdbc/with-transaction [txn conn]
      (f txn))))

(defn execute-fn! [datasource f]
  (with-open [conn (jdbc/get-connection datasource)]
    (f conn)))

(defn execute-sql! [conn sql]
  (jdbc/execute! conn sql))

(defn execute-stmt! [_ stmt params]
  (if params
    (jdbc/execute! (jdbcp/set-parameters stmt params))
    (jdbc/execute! stmt)))
