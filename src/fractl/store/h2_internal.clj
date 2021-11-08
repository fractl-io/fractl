(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [clojure.string :as s]
            [fractl.util.seq :as us]
            [fractl.component :as cn]
            [fractl.store.util :as su])
  (:import [java.sql PreparedStatement]))

(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id attrval]]))

(defn upsert-inst-statement [conn table-name id obj]
  (if (cn/relational-schema?)
    (let [[entity-name instance] obj
          uk-attrs (cn/unique-attributes (su/find-entity-schema entity-name))
          id-attr-nms (s/join "," (mapv name uk-attrs))
          ks (keys (cn/instance-attributes instance))
          col-names (map name ks)
          col-vals (map #(% instance) ks)
          sql (str "MERGE INTO " table-name "("
                   (us/join-as-string col-names ", ")
                   ") KEY (" id-attr-nms ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")")]
      [(jdbc/prepare conn [sql]) col-vals])
    (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
          ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
      [pstmt [id obj]])))
