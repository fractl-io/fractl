(ns agentlang.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [clojure.string :as s]
            [agentlang.util.seq :as us]
            [agentlang.component :as cn]
            [agentlang.store.util :as su])
  (:import [java.sql PreparedStatement]))

(defn upsert-inst-statement [conn table-name id obj]
  (let [[entity-name instance] obj
        uk-attrs (cn/unique-attributes (su/find-entity-schema entity-name))
        id-attr-nms (s/join "," (mapv #(str "_" (name %)) uk-attrs))
        ks (keys (cn/instance-attributes instance))
        col-names (mapv #(str "_" (name %)) ks)
        col-vals (mapv #(% instance) ks)
        sql (str "MERGE INTO " table-name "("
                 (us/join-as-string col-names ", ")
                 ") KEY (" id-attr-nms ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")")]
    [(jdbc/prepare conn [sql]) col-vals]))
