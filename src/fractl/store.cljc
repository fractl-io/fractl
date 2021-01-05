(ns fractl.store
  (:require #?(:clj [fractl.store.h2 :as h2]
               :cljs [fractl.store.alasql :as alasq])
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

(defn open-default-store
  ([store-config]
   #?(:clj (make-default-store store-config (h2/make))
      :cljs (make-default-store store-config (alasq/make))))
  ([] #?(:clj (make-default-store nil (h2/make))
         :cljs (make-default-store nil (alasq/make)))))

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def create-schema p/create-schema)
(def drop-schema p/drop-schema)
(def upsert-instance p/upsert-instance)
(def delete-by-id p/delete-by-id)
(def query-by-id p/query-by-id)
(def query-all p/query-all)
(def do-query p/do-query)
(def compile-query p/compile-query)

(defn upsert-instances [store record-name insts]
  (doseq [inst insts]
    (p/upsert-instance store record-name inst))
  insts)
