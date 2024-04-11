(ns fractl.resolver.click-house
  (:require [clojure.string :as s]
            [fractl.lang.internal :as li]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.store.util :as stu]
            [fractl.store.jdbc-cp :as cp])
  (:import [java.sql Connection DriverManager PreparedStatement
            ResultSet SQLException Statement]
           [javax.sql DataSource]
           [java.util Collections Properties]
           [com.clickhouse.jdbc ClickHouseDataSource]))

(defn- ch-create [ds instance]
  instance)

(defn- ch-update [ds instance]
  instance)

(defn- ch-delete [ds arg]
  arg)

(defn- ch-query [ds [entity-name clause]]
  nil)

(defn- as-ch-type [tp]
  (case tp
    (:String :Keyword :Email :Password
             :Time :Edn :Any :Path) "String"
    :Date "Date"
    :DateTime "DateTime"
    (:UUID :Identity) "UUID"
    (:Int :Int64 :Integer :BigInteger) "Int64"
    :Float "Float32"
    (:Double :Decimal) "Float64"
    :Boolean "Boolean"
    :Map "Map"
    :List "Array"
    (u/throw-ex (str "cannot map " tp " to a click-house datatype"))))

(defn- create-table-sql [table-name entity-schema]
  (let [attrs (keys entity-schema)
        atypes (mapv (fn [a] [a (as-ch-type (cn/attribute-type entity-schema a))]) attrs)]
    (str "CREATE TABLE IF NOT EXISTS " table-name " ("
         (reduce (fn [s [k v]] (str s (when (seq s) ", ") (name k) v)) "" atypes)
         ")")))

(def ^:private inited-components (atom #{}))

(defn- ch-on-set-path [ds [_ path]]
  (let [[c n] (li/split-path path)
        ^Connection conn (ds)
        ^Statement stmt (.createStatement conn)
        dbname (s/replace (name c) "." "_")]
    (try
      (do 
        (when-not (some #{c} @inited-components)
          (let [sql (str "CREATE DATABASE IF NOT EXISTS " dbname)]
            (.execute stmt sql)
            (swap! inited-components conj c)))
        (let [table-name (str dbname "." (name n))
              scm (stu/find-entity-schema path)
              sql (create-table-sql table-name scm)]
          (.execute stmt sql)
          path))
      (finally
        (.close conn)))))

(defn- ^Properties as-ch-props [a-map]
  (let [^Properties props (Properties.)]
    (doseq [[k v] a-map]
      (.set props (name k) v))
    props))

(defmake :click-house
  (fn [resolver-name config]
    (let [^Properties props (as-ch-props (:properties config))
          ^DataSource dsobj (cp/as-pooled
                             (ClickHouseDataSource. (:url config) props)
                             (get config :statement-cache-size 10))
          ds #(.getConnection dsobj (:username config) (:password config))
          handlers {:create (partial ch-create ds)
                    :update (partial ch-update ds)
                    :delete (partial ch-delete ds)
                    :query (partial ch-query ds)
                    :on-set-path (partial ch-on-set-path ds)}]
      (r/make-resolver resolver-name handlers))))
