(ns fractl.store.sfdc.persistence
  "Local data storage for SFDC objects"
  (:require [fractl.lang.internal :as li]
            [fractl.store.sfdc.format :as fmt]))

(def ^:private storage-root "unpackaged")
(def ^:private path-sep java.io.File/separator)

(def ^:private data-handlers
  {:Role {:folder-path (str storage-root path-sep "roles")
          :file-name #(str (:Name %) ".role")
          :meta-dissoc #(dissoc % :Id :Name)}})

(defn- folder-path [recname]
  (get-in data-handlers [recname :folder-path]))

(defn- dissoc-meta-fields [recname inst]
  ((get-in data-handlers [recname :meta-dissoc]) inst))

(defn- object-file-name [recname inst]
  ((get-in data-handlers [recname :file-name]) inst))

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
