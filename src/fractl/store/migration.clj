(ns fractl.store.migration
  (:require [clojure.set :as set]
            [fractl.util.seq :as su]
            [fractl.lang.raw :as raw]
            [fractl.lang.internal :as li]
            [fractl.store.protocol :as p])
  (:import [liquibase Contexts Liquibase]
           [liquibase.database Database DatabaseFactory]
           [liquibase.database.jvm JdbcConnection]
           [liquibase.exception LiquibaseException]
           [liquibase.resource FileSystemResourceAccessor]
           [java.io File]
           [java.sql Connection DriverManager SQLException]))

(defn lqb-init [store conf]
  (let [{db-url :url username :username password :password}
        (p/parse-connection-info store conf)
        conn #(DriverManager/getConnection db-url username password)]
    {:connection conn}))

(defn lqb-migrate [lqb]
  (let [^Connection connection ((:connection lqb))]
    (try
      (let [^Database database (.findCorrectDatabaseImplementation
                                (DatabaseFactory/getInstance)
                                (JdbcConnection. connection))
            ^File base-dir (File. ".")
            ^Liquibase liquibase (Liquibase. "db/migration/changelog.sql"
                                             (FileSystemResourceAccessor. (into-array File [base-dir]))
                                             database)]
        (.update liquibase (Contexts.)))
      (catch Exception ex
        (when connection
          (.rollback connection))
        (throw ex))
      (finally
        (when connection
          (.close connection))))))

(def init lqb-init)
(def migrate lqb-migrate)

(defn- entity-def? [obj]
  (let [n (first obj)]
    (or (= n 'entity)
        (and (= n 'relationship)
             (let [s (second obj)
                   scm (if (map? s)
                         (li/record-attributes s)
                         (nth obj 2))]
               (:between (:meta scm)))))))

(defn- extract-entity-names [defs]
  (set (mapv #(li/record-name (second %)) (filter entity-def? defs))))

(defn- find-entity-def [entity-name defs]
  (when-let [d (first (filter #(and (= 'entity (first %))
                                    (let [s (second %)
                                          recname (if (map? s)
                                                    (li/record-name s)
                                                    s)]
                                      (= entity-name recname)))
                              defs))]
    (li/only-user-attributes
     (let [spec (second d)]
       (if (map? spec)
         (li/record-attributes spec)
         (nth d 2))))))

(defn- normalize-attr-spec [spec]
  (if (keyword? spec)
    {:type spec}
    spec))

(defn- attribute-alteration [new-entity-spec old-entity-spec attr-name]
  (let [ninfo (normalize-attr-spec (attr-name new-entity-spec))
        oinfo (normalize-attr-spec (attr-name old-entity-spec))]
    (when-not (= ninfo oinfo)
      (merge nil (when (not= (:type ninfo) (:type oinfo))
                   {:type (:type ninfo)})
             (when (and (:indexed ninfo) (not (:indexed oinfo)))
               {:indexed true})
             (when (and (:unique ninfo) (not (:unique oinfo)))
               {:unique true})
             (when (and (not (:unique ninfo)) (:unique oinfo))
               {:unique false})))))

(defn- make-alter-diff [entities new-defs old-defs]
  (doseq [e entities]
    (let [new (find-entity-def e new-defs)
          old (find-entity-def e old-defs)
          nks (set (keys new)), oks (set (keys old))
          common-attrs (set/intersection nks oks)
          alter-attrs (seq
                       (su/nonils
                        (mapv
                         (partial attribute-alteration new old)
                         common-attrs)))
          add-attrs (seq (set/difference nks oks))
          drop-attrs (seq (set/difference oks nks))]
      (when (or alter-attrs add-attrs drop-attrs)
        {e {:add (when add-attrs (mapv #(% new) add-attrs))
            :drop drop-attrs
            :alter alter-attrs}}))))

(defn diff [component old-def]
  (let [new-def (rest (raw/as-edn component))
        new-entities (extract-entity-names new-def)
        old-entities (extract-entity-names old-def)
        active-entities (set/intersection old-entities new-entities)]
    (println {:drop (seq (set/difference old-entities new-entities))
              :alter (make-alter-diff active-entities new-def old-def)})))
