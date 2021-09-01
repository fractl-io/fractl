(ns fractl.resolver.data-sync
  (:require [fractl.resolver.core :as r]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.component :as cn]
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
    (:Kernel/Int
     :Kernel/Int64 
     :Kernel/BigInteger
     :Kernel/Float
     :Kernel/Double
     :Kernel/Decimal
     :Kernel/Boolean)
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
   
(defn- file-import [spec]
  ;; TODO: Upsert new instances in DB.
  (let [contents (csv/read-csv (:FilePath spec))
        entity-name (:TargetEntity spec)]
    (if-let [attr-map (attribute-mapping-as-indices
                       (:AttributeMapping spec)
                       (first contents)
                       (cn/entity-schema entity-name))]
      (records-as-instances
       entity-name
       attr-map
       (rest contents))
      (u/throw-ex
       (str
        "no valid attribute mapping for "
        (:TargetEntity spec))))))

(defn- file-export [spec]
  ;; TODO: implement
  )

(defn- data-sync [spec]
  )

(defn- data-sync-eval [inst]
  (case (cn/instance-name inst)
    [:Kernel :DataSync] (data-sync inst)
    (u/throw-ex
     (str "data-sync resolver cannot handle "
          (cn/instance-name inst)))))

(def ^:private resolver-fns
  {:eval {:handler data-sync-eval}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
