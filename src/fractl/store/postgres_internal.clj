(ns fractl.store.postgres-internal
  (:require [next.jdbc :as jdbc]
            [clojure.set :as set]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.component :as cn])
  (:import [java.sql PreparedStatement]))

(defn- set-excluded-columns [col-names]
  (loop [cs col-names, s ""]
    (if-let [c (first cs)]
      (recur (rest cs)
             (str s " " c " = EXCLUDED." c
                  (when (seq (rest cs))
                    ", ")))
      s)))

(defn upsert-inst-statement [conn table-name id obj]
  (let [[entity-name instance] obj
        id-attr (cn/identity-attribute-name entity-name)
        id-attr-nm (str "_" (name id-attr))
        ks (keys (cn/instance-attributes instance))
        col-names (mapv #(str "_" (name %)) ks)
        col-vals (u/objects-as-string (mapv #(% instance) ks))
        sql (str "INSERT INTO " table-name " ("
                 (us/join-as-string col-names ", ")
                 ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")  ON CONFLICT (" id-attr-nm ") DO UPDATE SET"
                 (set-excluded-columns
                  (set/difference (set col-names) #{id-attr-nm})))]
    [(jdbc/prepare conn [sql]) col-vals]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setObject pstmt 1 id)
    [pstmt nil]))

(defn delete-index-statement [conn table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE _" cn/slc-id-attr " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id)]]))

(defn delete-by-id-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE _" cn/slc-id-attr " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id)]]))
