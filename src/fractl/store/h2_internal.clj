(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc])
  (:import [java.sql PreparedStatement]))

(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id attrval]]))

(defn upsert-inst-statement [conn table-name id obj]
  (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id obj]]))
