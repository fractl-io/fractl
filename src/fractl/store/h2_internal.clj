(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp])
  (:import [java.sql PreparedStatement]))

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

(defn do-query-statement [conn query-sql query-params]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (jdbcp/set-parameters pstmt query-params)
    [pstmt nil]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setString pstmt 1 (str id))
    [pstmt nil]))

(defn transact! [datasource f]
  (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (f txn))))

(defn execute-sql! [conn sql]
  (jdbc/execute! conn sql))

(defn execute-stmt! [_ stmt _]
  (jdbc/execute! stmt))