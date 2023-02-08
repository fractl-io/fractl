(ns fractl.resolver.data-sync
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.component :as cn]
            [fractl.store :as store]
            [fractl.lang.internal :as li]
            [fractl.evaluator.state :as es]
            [fractl.datafmt.csv :as csv]))

(defn- normalize-attribute-names [entity-schema attr-map]
  (into
   {}
   (mapv
    (fn [[k v]]
      [k
       (let [aname (if (keyword? v)
                     v
                     (keyword v))]
         [aname (cn/attribute-type entity-schema aname)])])
    attr-map)))

(defn- attribute-mapping-as-indices [attr-map titles entity-schema]
  (when (map? attr-map)
    (normalize-attribute-names
     entity-schema
     (if (int? (first (keys attr-map)))
       attr-map
       (mapv
        (fn [[k v]]
          (if-let [i (su/index-of k titles)]
            [i v]
            (u/throw-ex (str "cannot map " v " to " k))))
        attr-map)))))

(defn- parse-attribute-value [v attr-type]
  (case attr-type
    (:Kernel.Lang/Int
     :Kernel.Lang/Int64
     :Kernel.Lang/BigInteger
     :Kernel.Lang/Float
     :Kernel.Lang/Double
     :Kernel.Lang/Decimal
     :Kernel.Lang/Boolean)
    (read-string v)
    v))

(defn- record-as-instance [entity-name attr-map record]
  (loop [attr-map attr-map, result {}]
    (if-let [[i [attr-name attr-type]] (first attr-map)]
      (recur
       (rest attr-map)
       (if-let [v (get record i)]
         (assoc result attr-name (parse-attribute-value v attr-type))
         result))
      (cn/make-instance entity-name result))))

(defn- records-as-instances [entity-name attr-map records]
  (mapv (partial record-as-instance entity-name attr-map) records))

(defn- upsert-instances [insts]
  (let [ev (es/get-active-evaluator)
        [component entity-name] (li/split-path (cn/instance-type (first insts)))
        event-name (li/make-path
                    [component
                     (keyword (str "Upsert_" (subs (str entity-name) 1)))])]
    (doseq [inst insts]
      (ev (cn/make-instance
           {event-name
            {:Instance inst}})))
    insts))

(defn- file-import [spec]
  (let [contents (csv/read-csv (:FilePath spec))
        entity-name (:Entity spec)]
    (if-let [attr-map (attribute-mapping-as-indices
                       (:AttributeMapping spec)
                       (first contents)
                       (cn/entity-schema entity-name))]
      (upsert-instances
       (records-as-instances
        entity-name
        attr-map
        (rest contents)))
      (u/throw-ex
       (str
        "no valid attribute mapping for "
        (:Entity spec))))))

(defn- attribute-map-for-export [attr-map]
  (into
   {}
   (mapv
    (fn [[k v]]
      [(if (keyword? k)
         k
         (keyword k))
       v])
    attr-map)))

(defn- emit-titles [attr-map ks]
  (let [s (reduce
           #(str %1 (get attr-map %2) ",")
           "" ks)]
    (subs s 0 (dec (count s)))))

(defn- emit-row [attr-map ks row]
  (let [s (reduce
           #(str %1 (get row %2) ",")
           "" ks)]
    (subs s 0 (dec (count s)))))

(defn- instances-to-csv [spec rows]
  (let [attr-map (attribute-map-for-export (:AttributeMapping spec))
        ks (keys attr-map)
        titles (str (emit-titles attr-map ks) u/line-sep)
        er (partial emit-row attr-map ks)]
    (loop [rows rows, s ""]
      (if-let [row (first rows)]
        (recur (rest rows) (str s (er row) u/line-sep))
        (str titles s)))))

(defn- file-export [spec]
  (let [entity-name (li/split-path (:Entity spec))
        store (es/get-active-store)
        query (store/compile-query
               store
               {:from entity-name :where :*})
        rows (store/query-all store entity-name (first query))
        out-file (:FilePath spec)]
    (spit out-file (instances-to-csv spec rows))
    out-file))

(def ^:private file-uri-prefix "file://")
(def ^:private file-uri-prefix-length (count file-uri-prefix))

(defn- file-path-from-uri [^String uri]
  (.substring uri file-uri-prefix-length))

(defn- do-data-import [^String uri src-spec]
  (if (.startsWith uri file-uri-prefix)
    (file-import (assoc src-spec :FilePath (file-path-from-uri uri)))
    (u/throw-ex (str "source not supported: " uri))))

(defn- do-data-export [src-spec ^String dest-uri]
  (if (.startsWith dest-uri file-uri-prefix)
    (file-export (assoc src-spec :FilePath (file-path-from-uri dest-uri)))
    (u/throw-ex (str "destination type not supported: " dest-uri))))

(defn- data-sync [spec]
  (let [src (:Source spec)]
    (if-let [uri (:Uri src)]
      (do-data-import uri src)
      (if-let [dest (:DestinationUri spec)]
        (do-data-export src dest)
        (u/throw-ex
         "destination required for data-sync")))))

(defn- data-sync-eval [inst]
  (case (cn/instance-type inst)
    [:Kernel.Lang :DataSync] (data-sync inst)
    (u/throw-ex
     (str "data-sync resolver cannot handle "
          (cn/instance-type inst)))))

(def ^:private resolver-fns
  {:eval {:handler data-sync-eval}})

(defmake :data-sync
  (fn [resolver-name _]
    (r/make-resolver resolver-name resolver-fns)))
