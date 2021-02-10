(ns fractl.store
  (:require #?(:clj [fractl.store.h2 :as h2]
               :cljs [fractl.store.alasql :as alasql])
            #?(:cljs [fractl.store.reagent.core :as reagent])
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
       (p/open-connection store (or store-config
                                    (make-default-store-config)))
       store)))

(def ^:private store-constructors
  #?(:clj
     {:h2 h2/make}
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

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def connection-info p/connection-info)
(def create-schema p/create-schema)
(def drop-schema p/drop-schema)
(def upsert-instance p/upsert-instance)
(def delete-by-id p/delete-by-id)
(def query-by-id p/query-by-id)
(def query-all p/query-all)
(def do-query p/do-query)
(def compile-query p/compile-query)
(def get-reference p/get-reference)

(defn reactive?
  "Checks whether a given store supports reactive references"
  [store]
  (if-let [conn-info (connection-info store)]
    (when (map? conn-info)
      (get conn-info :reactive))
    false))

(defn upsert-instances [store record-name insts]
  (doseq [inst insts]
    (p/upsert-instance store record-name inst))
  insts)
