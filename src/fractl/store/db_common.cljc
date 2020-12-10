(ns fractl.store.db-common
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.store.util :as su]          
            [fractl.store.db-internal :as dbi]
            [fractl.store.sql :as sql]
            #?(:clj [fractl.store.h2-internal :as h2i]
               :cljs [fractl.store.alasql-internal :as aqi])))

#?(:clj
   (def ^:private store-fns
     {:transact! h2i/transact!
      :execute-sql! h2i/execute-sql!
      :execute-stmt! h2i/execute-stmt!
      :create-entity-table-sql h2i/create-entity-table-sql
      :create-index-table-sql h2i/create-index-table-sql
      :create-identity-index-sql h2i/create-identity-index-sql
      :upsert-inst-statement h2i/upsert-inst-statement
      :upsert-index-statement h2i/upsert-index-statement
      :delete-inst-statement h2i/delete-inst-statement
      :delete-index-statement h2i/delete-index-statement
      :validate-ref-statement h2i/validate-ref-statement})
   :cljs
   (def ^:private store-fns
     {:transact! aqi/transact!
      :execute-sql! aqi/execute-sql!
      :execute-stmt! aqi/execute-stmt!
      :create-entity-table-sql aqi/create-entity-table-sql
      :create-index-table-sql aqi/create-index-table-sql
      :create-identity-index-sql aqi/create-identity-index-sql
      :upsert-inst-statement aqi/upsert-inst-statement
      :upsert-index-statement aqi/upsert-index-statement
      :delete-inst-statement aqi/delete-inst-statement
      :delete-index-statement aqi/delete-index-statement
      :validate-ref-statement aqi/validate-ref-statement}))

(def transact! (partial (:transact! store-fns)))
(def execute-sql! (partial (:execute-sql! store-fns)))
(def execute-stmt! (partial (:execute-stmt! store-fns)))
(def create-entity-table-sql (partial (:create-entity-table-sql store-fns)))
(def create-index-table-sql (partial (:create-index-table-sql store-fns)))
(def create-identity-index-sql (partial (:create-identity-index-sql store-fns)))
(def upsert-inst-statement (partial (:upsert-inst-statement store-fns)))
(def upsert-index-statement (partial (:upsert-index-statement store-fns)))
(def delete-inst-statement (partial (:delete-inst-statement store-fns)))
(def delete-index-statement (partial (:delete-index-statement store-fns)))
(def validate-ref-statement (partial (:validate-ref-statement store-fns)))

(defn- create-identity-index! [connection entity-table-name ident-attr]
  (let [sql (create-identity-index-sql entity-table-name (dbi/db-ident ident-attr))]
    (if (execute-sql! connection sql)
      entity-table-name
      (u/throw-ex (str "Failed to create index table for identity column - "
                       [entity-table-name ident-attr])))))

(defn- create-entity-table!
  "Create a table to store instances of an entity. As 'identity-attribute' is
  specified to be used as the primary-key in the table."
  [connection tabname ident-attr]
  (let [sql (create-entity-table-sql tabname ident-attr)]
    (if (execute-sql! connection sql)
      tabname
      (u/throw-ex (str "Failed to create table for " tabname)))))

(defn- create-index-table! [connection entity-schema entity-table-name attrname idxattr]
  (let [[tabsql idxsql] (create-index-table-sql
                         entity-table-name attrname
                         (sql/sql-index-type (cn/attribute-type entity-schema idxattr))
                         (cn/unique-attribute? entity-schema idxattr))]
    (when-not (and (execute-sql! connection tabsql)
                   (execute-sql! connection idxsql))
      (u/throw-ex (str "Failed to create lookup table for " [entity-table-name attrname])))))

(defn- create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [connection entity-schema entity-table-name ident-attr indexed-attrs]
  (create-entity-table! connection entity-table-name ident-attr)
  (when ident-attr
    (create-identity-index! connection entity-table-name ident-attr))
  (let [cit (partial create-index-table! connection entity-schema entity-table-name)]
    (doseq [idxattr indexed-attrs]
      (let [attrname (dbi/db-ident idxattr)]
        (cit attrname idxattr)))
    entity-table-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [connection db-schema-name]
  (if (seq (execute-sql! connection [(dbi/create-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [connection db-schema-name]
  (if (seq (execute-sql! connection [(dbi/drop-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (transact! datasource
               (fn [txn]
                 (create-db-schema! txn scmname)
                 (doseq [ename (cn/entity-names component-name)]
                   (let [tabname (dbi/table-for-entity ename)
                         schema (dbi/find-entity-schema ename)
                         indexed-attrs (cn/indexed-attributes schema)]
                     (create-tables! txn schema tabname :Id indexed-attrs)))))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (transact! datasource
               (fn [txn]
                 (drop-db-schema! txn scmname)))
    component-name))

(defn- upsert-indices!
  "Insert or update new index entries relevant for an entity instance.
  The index values are available in the `attrs` parameter."
  [conn entity-table-name indexed-attrs instance]
  (let [id (:Id instance)]
    (doseq [[attrname tabname] (dbi/index-table-names entity-table-name indexed-attrs)]
      (let [[pstmt params] (upsert-index-statement conn tabname (dbi/db-ident attrname)
                                                   id (attrname instance))]
        (execute-stmt! conn pstmt params)))))

(defn- validate-references! [conn inst ref-attrs]
  (doseq [[aname scmname] ref-attrs]
    (let [p (cn/find-ref-path scmname)
          component (:component p)
          entity-name (:record p)
          tabname (dbi/table-for-entity [component entity-name] (name component))
          rattr (first (:refs p))
          colname (name rattr)
          index-tabname (if (= rattr :Id) tabname (dbi/index-table-name tabname colname))
          [stmt params] (validate-ref-statement conn index-tabname colname (get inst aname))]
      (when-not (seq (execute-stmt! conn stmt params))
        (u/throw-ex (str "Reference not found - " aname ", " p))))))

(defn- upsert-inst!
  "Insert or update an entity instance."
  [conn table-name inst ref-attrs]
  #?(:cljs (.log js/console "upsert-inst! - table-name: " table-name))
  #_(when (seq ref-attrs)
    (validate-references! conn inst ref-attrs))
  (let [attrs (cn/serializable-attributes inst)
        id (:Id attrs)
        obj (su/clj->json (dissoc attrs :Id))
        [pstmt params] (upsert-inst-statement conn table-name id obj)]
    #?(:cljs (.log js/console (str "upsert statement: " pstmt)))
    (execute-stmt! conn pstmt params)))

(defn upsert-instance [datasource entity-name instance]
  (let [tabname (dbi/table-for-entity entity-name)
        entity-schema (dbi/find-entity-schema entity-name)
        indexed-attrs (cn/indexed-attributes entity-schema)
        ref-attrs (cn/ref-attribute-schemas entity-schema)]
    (transact! datasource
                    (fn [txn]
                      (upsert-inst! txn tabname instance ref-attrs)
                      (upsert-indices! txn tabname indexed-attrs instance)))
    instance))

(defn- delete-indices!
  "Delete index entries relevant for an entity instance."
  [conn entity-table-name indexed-attrs id]
  (let [index-tabnames (dbi/index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [[pstmt params] (delete-index-statement
                            conn tabname
                            (dbi/db-ident attrname) id)]
        (execute-stmt! conn pstmt params)))))

(defn- delete-inst!
  "Delete an entity instance."
  [conn tabname id]
  (let [[pstmt params] (delete-inst-statement conn tabname id)]
    (execute-stmt! conn pstmt params)))

(defn delete-instance [datasource entity-name instance]
  (let [id (:Id instance)
        tabname (dbi/table-for-entity entity-name)
        entity-schema (dbi/find-entity-schema entity-name)
        indexed-attrs (cn/indexed-attributes entity-schema)]
    (transact! datasource
               (fn [txn]
                 (delete-indices! txn tabname indexed-attrs id)
                 (delete-inst! txn tabname id)))
    id))

