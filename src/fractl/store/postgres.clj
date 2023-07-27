(ns fractl.store.postgres
  "The storage layer implementation for PostgresSQL."
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.protocol :as p]
            [fractl.store.util :as su]
            [fractl.store.jdbc-cp :as cp]
            [fractl.store.db-common :as db]
            [fractl.store.postgres-internal :as pi]
            [fractl.store.migration :as mg])
  (:import [org.postgresql.util PSQLException]))

(def ^:private driver-class "org.postgresql.Driver")
(def ^:private jdbc-url-prefix "jdbc:postgresql://")

(defn- maybe-uuid-from-str [x]
  (if (string? x)
    (u/uuid-from-string x)
    x))

(defn- table-names-from-schema [result]
  (mapv :pg_tables/tablename result))

(def ^:private
  type-lookup
  "Maps postgres to fractl types.
  Types currently unsupported in fractl are explicitly mapped to false."
  {"bigint" :Fractl.Kernel.Lang/Int64
   "int8" :Fractl.Kernel.Lang/Int64
   "bigserial" :Fractl.Kernel.Lang/Int64
   "serial8" :Fractl.Kernel.Lang/Int64
   "bit" false
   "bit varying" false
   "varbit" false
   "boolean" :Fractl.Kernel.Lang/Boolean
   "bool" :Fractl.Kernel.Lang/Boolean
   "box" false
   "bytea" false
   "binary data" false
   "character" :Fractl.Kernel.Lang/String
   "char" :Fractl.Kernel.Lang/String
   "character varying" :Fractl.Kernel.Lang/String
   "varchar" :Fractl.Kernel.Lang/String
   "cidr" false
   "circle" false
   "date" :Fractl.Kernel.Lang/String
   "double precision" :Fractl.Kernel.Lang/Double
   "float8" :Fractl.Kernel.Lang/Double
   "inet" false
   "integer" :Fractl.Kernel.Lang/Int
   "int" :Fractl.Kernel.Lang/Int
   "int4" :Fractl.Kernel.Lang/Int
   "interval" false
   "json" :Fractl.Kernel.Lang/String
   "jsonb" false
   "line" false
   "lseg" false
   "macaddr" false
   "money" :Fractl.Kernel.Lang/Decimal
   "numeric" :Fractl.Kernel.Lang/Decimal
   "decimal" :Fractl.Kernel.Lang/Decimal
   "path" false
   "pg_lsn" false
   "point" false
   "polygon" false
   "real" :Fractl.Kernel.Lang/Float
   "float4" :Fractl.Kernel.Lang/Float
   "smallint" :Fractl.Kernel.Lang/Int
   "int2" :Fractl.Kernel.Lang/Int
   "smallserial" :Fractl.Kernel.Lang/Int
   "serial2" :Fractl.Kernel.Lang/Int
   "serial" :Fractl.Kernel.Lang/Int
   "serial4" :Fractl.Kernel.Lang/Int
   "text" :Fractl.Kernel.Lang/String
   "time" :Fractl.Kernel.Lang/String
   "time with time zone" :Fractl.Kernel.Lang/String
   "timetz" :Fractl.Kernel.Lang/String
   "timestamp" :Fractl.Kernel.Lang/String
   "timestamp with time zone" :Fractl.Kernel.Lang/String
   "timestamptz" :Fractl.Kernel.Lang/String
   "tsquery" :Fractl.Kernel.Lang/String
   "tsvector" false
   "txid_snapshot" false
   "uuid" :Fractl.Kernel.Lang/UUID
   "xml" :Fractl.Kernel.Lang/String})

(def ^:private fetch-schema-sql
  "select * from pg_catalog.pg_tables where schemaname<>'pg_catalog' and schemaname<>'information_schema'")

(def ^:private fetch-columns-sql
  "select column_name, data_type from information_schema.columns where table_name = ?")

(def ^:private fetch-pk-columns-sql
  (str "select a.attname, format_type(a.atttypid, a.atttypmod) as data_type "
       "from pg_index i "
       "join pg_attribute a ON a.attrelid = i.indrelid and a.attnum = any(i.indkey) "
       "where i.indrelid = '?'::regclass "
       "and i.indisprimary"))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              jdbc-url (str jdbc-url-prefix
                            (or (:host connection-info)
                                (System/getenv "POSTGRES_HOST")
                                "localhost")
                            "/"
                            (or (:dbname connection-info)
                                (System/getenv "POSTGRES_DB")
                                "postgres")
                            "?stringtype=unspecified")
              username (or (:username connection-info)
                           (System/getenv "POSTGRES_USER")
                           "postgres")
              password (or (:password connection-info)
                           (System/getenv "POSTGRES_PASSWORD"))]
          (when (mg/migrate jdbc-url username password)
            (u/safe-set-once
             datasource
             #(let [dbspec {:driver-class driver-class
                            :jdbc-url jdbc-url
                            :username username
                            :password password}]
                (cp/open-pooled-datasource dbspec)))
            true)))
      (close-connection [_]
        (try
          (do (u/call-and-set
               datasource
               #(when @datasource
                  (cp/close-pooled-datasource @datasource)
                  nil))
              true)
          (catch Exception _ false)))
      (connection-info [_]
        (or @datasource {}))
      (create-schema [_ component-name]
        (db/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (db/drop-schema @datasource component-name))
      (fetch-schema [_]
        (db/fetch-schema
         @datasource fetch-schema-sql
         table-names-from-schema fetch-columns-sql
         fetch-pk-columns-sql type-lookup))
      (upsert-instance [_ entity-name instance]
        (db/upsert-instance
         pi/upsert-inst-statement
         @datasource entity-name instance))
      (create-instance [_ entity-name instance]
        (db/create-instance @datasource entity-name instance))
      (update-instance [_ entity-name instance]
        (db/update-instance @datasource entity-name instance))
      (delete-by-id [_ entity-name id-attr-name id]
        (db/delete-by-id
         pi/delete-by-id-statement
         @datasource entity-name id-attr-name id))
      (delete-all [_ entity-name purge]
        (db/delete-all @datasource entity-name purge))
      (delete-children [_ entity-name path]
        (db/delete-children @datasource entity-name path))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id
         pi/query-by-id-statement
         @datasource entity-name query (map maybe-uuid-from-str ids)))
      (query-by-unique-keys [_ entity-name unique-keys unique-values]
        (db/query-by-unique-keys
         pi/query-by-id-statement @datasource
         entity-name unique-keys unique-values))
      (query-all [_ entity-name query]
        (db/query-all @datasource entity-name query))
      (do-query [_ query params]
        (db/do-query @datasource query params))
      (call-in-transaction [_ f]
        (db/transact-fn! @datasource f))
      (compile-query [_ query-pattern]
        (db/compile-query query-pattern))
      (get-reference [_ path refs]))))
