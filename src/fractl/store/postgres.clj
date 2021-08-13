(ns fractl.store.postgres
  "The storage layer implementation for PostgresSQL."
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.protocol :as p]
            [fractl.store.util :as su]
            [fractl.store.jdbc-cp :as cp]
            [fractl.store.db-common :as db]
            [fractl.store.postgres-internal :as pi]))

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
  {"bigint" :Kernel/Int64
   "int8" :Kernel/Int64
   "bigserial" :Kernel/Int64
   "serial8" :Kernel/Int64
   "bit" false
   "bit varying" false
   "varbit" false
   "boolean" :Kernel/Boolean
   "bool" :Kernel/Boolean
   "box" false
   "bytea" false
   "binary data" false
   "character" :Kernel/String
   "char" :Kernel/String
   "character varying" :Kernel/String
   "varchar" :Kernel/String
   "cidr" false
   "circle" false
   "date" :Kernel/String
   "double precision" :Kernel/Double
   "float8" :Kernel/Double
   "inet" false
   "integer" :Kernel/Int
   "int" :Kernel/Int
   "int4" :Kernel/Int
   "interval" false
   "json" :Kernel/String
   "jsonb" false
   "line" false
   "lseg" false
   "macaddr" false
   "money" :Kernel/Decimal
   "numeric" :Kernel/Decimal
   "decimal" :Kernel/Decimal
   "path" false
   "pg_lsn" false
   "point" false
   "polygon" false
   "real" :Kernel/Float
   "float4" :Kernel/Float
   "smallint" :Kernel/Int
   "int2" :Kernel/Int
   "smallserial" :Kernel/Int
   "serial2" :Kernel/Int
   "serial" :Kernel/Int
   "serial4" :Kernel/Int
   "text" :Kernel/String
   "time" :Kernel/String
   "time with time zone" :Kernel/String
   "timetz" :Kernel/String
   "timestamp" :Kernel/String
   "timestamp with time zone" :Kernel/String
   "timestamptz" :Kernel/String
   "tsquery" :Kernel/String
   "tsvector" false
   "txid_snapshot" false
   "uuid" :Kernel/UUID
   "xml" :Kernel/String})

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
                            (or (:host connection-info) "localhost")
                            "/"
                            (or (:dbname connection-info) "v8")
                            "?stringtype=unspecified")
              username (or (:username connection-info) "postgres")
              password (or (:password connection-info) "posterV8")]
          (u/safe-set-once
           datasource
           #(let [dbspec {:driver-class driver-class
                          :jdbc-url jdbc-url
                          :username username
                          :password password}]
              (cp/open-pooled-datasource dbspec)))
          true))
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
        (if (or (cn/has-dynamic-entity-flag? instance)
                (cn/dynamic-entity? entity-name))
          (db/upsert-dynamic-entity-instance
           @datasource entity-name instance)
          (db/upsert-instance
           pi/upsert-inst-statement pi/upsert-index-statement
           @datasource entity-name instance true)))
      (update-instance [_ entity-name instance]
        (db/update-instance pi/upsert-inst-statement pi/upsert-index-statement @datasource entity-name instance))
      (delete-by-id [_ entity-name id]
        (db/delete-by-id pi/delete-by-id-statement pi/delete-index-statement @datasource entity-name id))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id
         pi/query-by-id-statement
         @datasource entity-name query (map maybe-uuid-from-str ids)))
      (query-by-unique-keys [_ entity-name unique-keys unique-values]
        (db/query-by-unique-keys
         pi/query-by-id-statement @datasource
         entity-name unique-keys unique-values))
      (query-all [_ entity-name query]
        (if (cn/dynamic-entity? entity-name)
          (db/query-all-dynamic @datasource entity-name query)
          (db/query-all @datasource entity-name query)))
      (do-query [_ query params]
        (db/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (if (:dynamic query-pattern)
          (db/compile-to-direct-query (:query query-pattern))
          (db/compile-to-indexed-query query-pattern)))
      (get-reference [_ path refs]))))
