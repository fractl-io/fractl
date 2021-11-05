(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [fractl.util.seq :as us]
            [fractl.component :as cn])
  (:import [java.sql PreparedStatement]))

(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id attrval]]))

(defn upsert-inst-statement [conn table-name id obj]
  (if (cn/relational-schema?)
    (let [[entity-name instance] obj
          id-attr (cn/identity-attribute-name entity-name)
          id-attr-nm (name id-attr)
          ks (keys (cn/instance-attributes instance))
          col-names (map name ks)
          col-vals (map #(% instance) ks)
          sql (str "MERGE INTO " table-name "("
                   (us/join-as-string col-names ", ")
                   ") KEY (" id-attr-nm ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")")]
      [(jdbc/prepare conn [sql]) col-vals])
    (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
          ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
      [pstmt [id obj]])))
