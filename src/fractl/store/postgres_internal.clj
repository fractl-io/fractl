(ns fractl.store.postgres-internal
  (:require [next.jdbc :as jdbc]
            [clojure.set :as set]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.component :as cn])
  (:import [java.sql PreparedStatement]))

(defn upsert-index-statement [conn table-name attr-col-name id attrval]
  (let [sql (str "INSERT INTO " table-name " (id, " attr-col-name ") VALUES (?, ?) "
                 "ON CONFLICT (id) DO UPDATE SET " attr-col-name " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id) attrval attrval]]))

(defn- set-excluded-columns [col-names]
  (loop [cs col-names, s ""]
    (if-let [c (first cs)]
      (recur (rest cs)
             (str s " " c " = EXCLUDED." c
                  (when (seq (rest cs))
                    ", ")))
      s)))

(defn upsert-inst-statement [conn table-name id obj]
  (if (cn/relational-schema?)
    (let [[entity-name instance] obj
          id-attr (cn/identity-attribute-name entity-name)
          id-attr-nm (name id-attr)
          ks (keys (cn/instance-attributes instance))
          col-names (mapv name ks)
          col-vals (mapv #(% instance) ks)
          sql (str "INSERT INTO " table-name "("
                 (us/join-as-string col-names ", ")
                 ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")  ON CONFLICT (" id-attr-nm ") DO UPDATE SET"
                 (set-excluded-columns
                  (set/difference (set col-names) #{id-attr-nm})))]
      [(jdbc/prepare conn [sql]) col-vals])
    (let [sql (str "INSERT INTO " table-name " (id, instance_json) VALUES (?, ?) "
                   "ON CONFLICT (id) DO UPDATE SET instance_json = ?")
          ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
      [pstmt [(u/uuid-from-string id) obj obj]])))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setObject pstmt 1 id)
    [pstmt nil]))

(defn delete-index-statement [conn table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id)]]))

(defn delete-by-id-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string id)]]))
