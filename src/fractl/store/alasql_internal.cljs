(ns fractl.store.alasql-internal
  (:require [cljsjs.alasql]
            [fractl.util :as u]
            [fractl.store.sql :as sql]
            [fractl.store.util :as su]))

(defn create-db [name]
  (js/alasql (str "CREATE DATABASE IF NOT EXISTS " name))
  (let [db (. js/alasql Database name)]
    (js/alasql (str "USE " name))
    db))

(defn upsert-index-statement [_ table-name _ id attrval]
  (let [sql (str "INSERT OR REPLACE INTO " table-name " VALUES (?, ?)")]
    [sql #js [id attrval]]))

(defn upsert-inst-statement [_ table-name id obj]
  (let [sql (str "INSERT OR REPLACE INTO " table-name " VALUES(?, ?)")]
    [sql #js [id obj]]))

(defn delete-index-statement [_ table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")]
    [sql #js [id]]))

(defn delete-inst-statement [_ table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")]
    [sql #js [id]]))

(defn query-by-id-statement [_ query-sql id]
  (let [stmt (str query-sql " * " (str id))]
    [stmt nil]))

(defn query-by-id [datasource entity-name query-sql ids]
  (let [[id-key json-key] (su/make-result-keys entity-name)]
    ((partial su/results-as-instances entity-name id-key json-key)
     (flatten (map #(let [pstmt (query-by-id-statement datasource query-sql %)]
                      pstmt
                      (set ids)))))))

(defn validate-ref-statement [_ index-tabname colname ref]
  (let [sql (str "SELECT 1 FROM " index-tabname " WHERE " colname " = ?")]
    [sql #js [ref]]))

(defn do-query-statement [_ query-sql query-params]
    [query-sql #js [query-params]])

(def compile-to-indexed-query (partial sql/compile-to-indexed-query
                                       su/table-for-entity
                                       su/index-table-name))

(defn execute-fn! [db f]
  (f db))

(defn execute-sql! [db sqls]
  (doseq [sql sqls]
    (when-not (.exec db sql)
      (u/throw-ex (str "Failed to execute sql statement - " sql))))
  true)

(defn execute-stmt! [db stmt params]
  (if params
    (.exec db stmt params)
    (.exec db stmt)))
