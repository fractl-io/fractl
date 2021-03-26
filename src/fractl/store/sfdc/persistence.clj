(ns fractl.store.sfdc.persistence
  "Local data storage for SFDC objects"
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.store.sfdc.format :as fmt])
  (:import [java.io File FilenameFilter]))

(def ^:private storage-root "unpackaged")
(def ^:private path-sep File/separator)

(def ^:private io-config
  {:Role {:extn ".role"
          :folder-path (str storage-root path-sep "roles")
          :file-name #(str (:FullName %) ".role")
          :meta-dissoc #(dissoc % :Id :FullName)
          :parser fmt/parse-role}})

(defn- object-file-extension [recname]
  (get-in io-config [recname :extn]))

(defn- folder-path [recname]
  (get-in io-config [recname :folder-path]))

(defn- dissoc-meta-fields [recname inst]
  ((get-in io-config [recname :meta-dissoc]) inst))

(defn- object-file-name [recname inst]
  ((get-in io-config [recname :file-name]) inst))

(defn- instance-parser [recname]
  (get-in io-config [recname :parser]))

(defn- write-object-file [recname folder inst]
  (let [file-name (object-file-name recname inst)
        xml (fmt/instance-as-xml recname (dissoc-meta-fields recname inst))]
    (spit (str folder path-sep file-name) xml)))          

(defn write-object [entity-name instances]
  (let [[_ n] (li/split-path entity-name)
        folder (folder-path n)
        outfn (partial write-object-file n folder)]
    (doseq [inst instances]
      (outfn inst))
    instances))

(defn- predicate-from-conditions [conditions]
  (let [n (second conditions)
        v (nth conditions 2)]
    ;; TODO: support more operators, including logical and/or.
    (case (first conditions)
      := #(= (get % n) v)
      (u/throw-ex (str "unsupported operation - " conditions)))))

(defn- make-file-extension-filter [^String extn]
  (reify FilenameFilter
    (accept [this f file-name]
      (.endsWith file-name extn))))

(defn- load-all-objects [[_ recname :as full-recname]]
  (let [^File folder (File. (folder-path recname))
        files (.listFiles
               folder
               (make-file-extension-filter
                (object-file-extension recname)))
        parse (partial (instance-parser recname) full-recname)]
    (map
     #(let [s (str %)] (parse s (slurp s)))
     files)))

(defn filter-records [full-recname conditions]
  (filter (predicate-from-conditions conditions)
          (load-all-objects full-recname)))
