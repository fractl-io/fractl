(ns fractl.store.postgres-internal
  (:require [next.jdbc :as jdbc]
            [fractl.util :as u])
  (:import [java.sql PreparedStatement]))

(defn upsert-index-statement [conn table-name attr-col-name id attrval]
  (let [sql (str "INSERT INTO " table-name " (id, " attr-col-name ") VALUES (?, ?) "
                 "ON CONFLICT (id) DO UPDATE SET " attr-col-name " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id) attrval attrval]]))

(defn upsert-inst-statement [conn table-name id obj]
  (let [sql (str "INSERT INTO " table-name " (id, instance_json) VALUES (?, ?) "
                 "ON CONFLICT (id) DO UPDATE SET instance_json = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id) obj obj]]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setObject pstmt 1 id)
    [pstmt nil]))
