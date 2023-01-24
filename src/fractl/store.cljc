(ns fractl.store
  (:require #?(:clj [fractl.store.h2 :as h2]
               :cljs [fractl.store.alasql :as alasql])
            #?(:clj [fractl.store.postgres :as postgres])
            #?(:cljs [fractl.store.reagent.core :as reagent])
            [fractl.component :as cn]
            [fractl.util.logger :as log]
            [fractl.store.util :as su]
            [fractl.store.protocol :as p]
            [fractl.util :as u]))

(def ^:private default-store (u/make-cell))

(defn get-default-store []
  @default-store)

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
  (u/make-cell
   #?(:clj
      {:h2 h2/make
       :postgres postgres/make}
      :cljs
      {:alasql alasql/make
       :reagent reagent/make})))

(defn register-store [store-name constructor]
  (u/call-and-set
   store-constructors
   #(assoc @store-constructors store-name constructor)))

(defn- store-cons [config]
  (if-let [t (:type config)]
    (t @store-constructors)
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

(defn- maybe-remove-id [record-name uq-attrs]
  (if (> (count uq-attrs) 1)
    (let [id-attr (cn/identity-attribute-name record-name)]
      (vec (filter #(not= % id-attr) uq-attrs)))
    uq-attrs))

(defn- cast-attr-types [entity-schema attr-names instance]
  (reduce (fn [inst attr-n]
            (if (= :Kernel/Any (:type (cn/find-attribute-schema (attr-n entity-schema))))
              (assoc inst attr-n (str (attr-n inst)))
              inst))
          instance attr-names))

(defn upsert-instance [store record-name instance]
  (let [scm (su/find-entity-schema record-name)
        instance (cn/secure-attributes record-name instance scm)
        uq-attrs (concat
                  (cn/unique-attributes scm)
                  (cn/compound-unique-attributes record-name))]
    (if-let [old-instance (and (some (set uq-attrs) (set (keys instance)))
                               (p/query-by-unique-keys
                                store record-name (maybe-remove-id record-name uq-attrs)
                                (cast-attr-types scm uq-attrs instance)))]
      (let [new-instance
            (cn/validate-instance
             (p/update-instance
              store record-name
              (merge-non-unique
               old-instance instance
               (set (concat (cn/immutable-attributes scm) uq-attrs)))))]
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
(def delete-all p/delete-all)

(defn- empty-result-on-error [f]
  (try
    (f)
    #?(:clj
       (catch Exception e
         (log/error e)
         [])
       :cljs
       (catch js/Error e
         (log/error e)
         []))))

(defn query-by-id [store entity-name query-sql ids]
  (empty-result-on-error
   #(p/query-by-id store entity-name query-sql ids)))

(defn query-by-unique-keys [store entity-name unique-keys unique-values]
  (empty-result-on-error
   #(p/query-by-unique-keys store entity-name unique-keys unique-values)))

(defn query-all [store entity-name query-sql]
  (empty-result-on-error
   #(p/query-all store entity-name query-sql)))

(defn do-query [store query query-params]
  (empty-result-on-error
   #(p/do-query store query query-params)))

(def compile-query p/compile-query)
(def get-reference p/get-reference)
(def call-in-transaction p/call-in-transaction)

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

(defn lookup-by-id
  ([store entity-name id-attr-name id]
   (query-by-unique-keys store entity-name [id-attr-name] {id-attr-name id}))
  ([store entity-name id]
   (lookup-by-id store entity-name cn/id-attr id)))
