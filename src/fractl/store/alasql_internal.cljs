(ns fractl.store.alasql-internal
  (:require [cljsjs.alasql]
            [clojure.string :as str]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.component :as cn]
            [fractl.store.sql :as sql]
            [fractl.store.util :as su]))

(defn create-db [name]
  (js/alasql (str "CREATE DATABASE IF NOT EXISTS " name))
  (let [db (. js/alasql Database name)]
    (js/alasql (str "USE " name))
    db))

(defn upsert-index-statement [_ table-name _ id attrval]
  (let [sql (str "INSERT OR REPLACE INTO " table-name " VALUES (?, ?)")]
    [sql [id attrval]]))

(defn upsert-inst-statement [_ table-name id obj]
  (let [[entity-name instance] obj
        id-attr (cn/identity-attribute-name entity-name)
        id-attr-nm (name id-attr)
        attrs (cn/fetch-schema (cn/instance-name instance))
        ks (sort (keys attrs))
        col-names (mapv name ks)
        col-vals (u/objects-as-string (mapv #(or (% instance) "") ks))
        sql (str "INSERT OR REPLACE INTO " table-name " VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")")]
    [sql col-vals]))

(defn delete-by-id-statement [_ table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE _Id = ?")]
    [sql [id]]))

(defn query-by-id-statement [_ query-sql id]
    (let [stmt (str query-sql)]
      [stmt [id]]))

(defn query-by-id [datasource entity-name query-sql ids]
  ((partial su/results-as-instances entity-name)
   (flatten (map #(let [pstmt (query-by-id-statement datasource query-sql %)]
                    pstmt
                    (set ids))))))

(defn validate-ref-statement [_ index-tabname colname ref]
  (let [sql (str "SELECT 1 FROM " index-tabname " WHERE _" colname " = ?")]
    [sql [ref]]))

(defn do-query-statement [_ query-sql query-params]
  [(if (map? query-sql) (:query query-sql) query-sql) query-params])

(defn execute-fn! [db f]
  (f db))

(defn execute-sql! [db sqls]
  (doseq [sql sqls]
    (when-not (.exec db sql)
      (u/throw-ex (str "Failed to execute sql statement - " sql))))
  true)

(defn execute-stmt! [db stmt params]
  (let [result (if params
                 (.exec db stmt (clj->js params))
                 (.exec db stmt))]
    (js->clj result :keywordize-keys true)))
