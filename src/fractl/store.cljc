(ns fractl.store
  (:require #?(:clj [fractl.store.h2 :as h2]
               :cljs [fractl.store.alasql :as alasql])
            #?(:clj [fractl.store.postgres :as postgres])
            #?(:clj [fractl.store.sfdc.metadata :as sfdc-metadata])
            #?(:cljs [fractl.store.reagent.core :as reagent])
            [fractl.component :as cn]
            [fractl.store.util :as su]
            [fractl.store.protocol :as p]
            [fractl.util :as u]))

(def ^:private default-store (u/make-cell))

(defn- make-default-store-config []
  #?(:clj {:dbname (str "./fractl.db." (System/currentTimeMillis))}
     :cljs {:dbname (str (gensym "fractl_db"))}))

(defn- make-default-store [store-config store]
  ;; NOTE: The default db connection, if opened,
  ;; will last the lifetime of the app.
  (u/safe-set-once
    default-store
    #(do
       (p/open-connection
        store
        (or store-config
            (make-default-store-config)))
       store)))

(def ^:private store-constructors
  #?(:clj
     {:h2 h2/make
      :postgres postgres/make
      :sfdc-metadata sfdc-metadata/make}
     :cljs
     {:alasql alasql/make
      :reagent reagent/make}))

(defn- store-cons [config]
  (if-let [t (:type config)]
    (t store-constructors)
    #?(:clj h2/make
       :cljs reagent/make)))

(defn open-default-store
  ([store-config]
   (let [make-store (store-cons store-config)]
     #?(:clj (make-default-store store-config (make-store))
        :cljs (make-default-store store-config (make-store)))))
  ([]
   (open-default-store nil)))

(defn open-reagent-store
  ([store-config]
   #?(:clj (u/throw-ex (str "Reagent store not supported - " store-config))
      :cljs (make-default-store (assoc store-config :reactive true) (reagent/make))))
  ([]
   (open-reagent-store nil)))

(defn- merge-non-unique
  "Merge non-unique attributes from inst-b to inst-a.
   The resulting instance is used for updating the store."
  [inst-a inst-b unique-keys]
  (loop [ks (keys inst-a), result inst-a]
    (if-let [k (first ks)]
      (if-not (some #{k} unique-keys)
        (if (contains? inst-b k)
          (recur (rest ks) (assoc result k (get inst-b k)))
          (recur (rest ks) result))
        (recur (rest ks) result))
      result)))

(defn upsert-instance [store record-name instance]
  (let [uq-attrs (cn/unique-attributes
                  (su/find-entity-schema record-name))]
    (if-let [old-instance (and (some (set uq-attrs) (set (keys instance)))
                               (p/query-by-unique-keys store record-name uq-attrs instance))]
      (let [new-instance
            (cn/validate-instance
             (p/update-instance
              store record-name
              (merge-non-unique old-instance instance uq-attrs)))]
        {:transition
         {:from old-instance
          :to new-instance}})
      (p/upsert-instance
       store record-name
       (cn/validate-instance instance)))))

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def connection-info p/connection-info)
(def create-schema p/create-schema)
(def fetch-schema p/fetch-schema)
(def drop-schema p/drop-schema)
(def create-table p/create-table)
(def delete-by-id p/delete-by-id)
(def query-by-id p/query-by-id)
(def query-by-unique-keys p/query-by-unique-keys)
(def query-all p/query-all)
(def do-query p/do-query)
(def compile-query p/compile-query)
(def get-reference p/get-reference)
(def pull p/pull)
(def push p/push)

(defn reactive?
  "Checks whether a given store supports reactive references"
  [store]
  (if-let [conn-info (connection-info store)]
    (when (map? conn-info)
      (get conn-info :reactive))
    false))

(defn upsert-instances [store record-name insts]
  (mapv
   #(upsert-instance
     store record-name
     %)
   insts))

(defn get-default-compile-query []
  (when-let [store @default-store]
    (partial p/compile-query store)))

(defn lookup-by-id [store entity-name id]
  (query-by-unique-keys store entity-name [:Id] {:Id id}))
