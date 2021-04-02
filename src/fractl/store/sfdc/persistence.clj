(ns fractl.store.sfdc.persistence
  "Local data storage for SFDC objects"
  (:require [clojure.data.xml :as xml]
            [clojure.string :as s]
            [fractl.util :as u]
            [fractl.git :as git]
            [fractl.lang.internal :as li]
            [fractl.store.sfdc.metadata-types :as mt]
            [fractl.store.sfdc.format :as fmt])
  (:import [java.io File FilenameFilter]
           [fractl.filesystem Util]
           [fractl.filesystem Zip]))

(def ^:private storage-root "unpackaged")

(def ^:private path-sep File/separator)
(def ^:private path-sep-re-pattern (re-pattern path-sep))

(def manifest-file-name "package.xml")
(def ^:private deploy-root-path "deploy")

(defn- generic-io-config [type-name parser]
  (let [extn (str "." type-name)]
    {:extn extn
     :folder-path (str storage-root path-sep (str type-name "s"))
     :file-name #(str (:FullName %) extn)
     :meta-dissoc #(dissoc % :Id :FullName)
     :parser parser}))

(def ^:private io-config
  {:Role (generic-io-config "role" fmt/parse-role)
   :Profile (generic-io-config "profile" fmt/parse-profile)})

(defn- object-file-extension [recname]
  (get-in io-config [recname :extn]))

(defn- folder-path [recname repo-dir]
  (str repo-dir path-sep (get-in io-config [recname :folder-path])))

(defn- dissoc-meta-fields [recname inst]
  ((get-in io-config [recname :meta-dissoc]) inst))

(defn- object-file-name [recname inst]
  ((get-in io-config [recname :file-name]) inst))

(defn- instance-parser [recname]
  (get-in io-config [recname :parser]))

(def ^:private journal-file "sfdc-metadata.journal")

(defn- read-journal-entries []
  (try
    (read-string (slurp journal-file))
    (catch Exception _
      {})))

(defn- make-journal-entry [recname file-path]
  (let [entries (read-journal-entries)
        reclog (get entries recname #{})
        updated-entries (assoc entries recname
                               (conj reclog file-path))]
    (spit journal-file updated-entries)
    recname))

(defn- write-object-file [recname folder repo-dir inst]
  (let [file-name (object-file-name recname inst)
        xml (fmt/instance-as-xml recname (dissoc-meta-fields recname inst))
        full-path (str folder path-sep file-name)]
    ;; TODO: Make these steps atomic.
    (spit full-path xml)
    (git/add repo-dir [full-path])
    (make-journal-entry recname full-path)))

(defn write-object [entity-name instances repo-dir]
  (let [[_ n] (li/split-path entity-name)
        folder (folder-path n repo-dir)
        outfn (partial write-object-file n folder repo-dir)]
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

(defn- members-tag [n]
  (let [s (str (if (li/name? n)
                 (subs (str (second (li/split-path n))) 1)
                 n))]
    {:tag :members :content [s]}))

(def ^:private all-members (members-tag "*"))

(defn- types-tag [opt]
  (let [has-query (seqable? opt)
        [a b] (li/split-path (if has-query (first opt) opt))
        m (or b a)
        n (name m)]
    {:tag :types :content
     (concat
      (if has-query
        (concat [{:tag :name :content [n]}]
                (if (seqable? (second opt))
                  (map members-tag (second opt))
                  [(members-tag (second opt))]))
        [{:tag :name :content [n]} all-members]))}))

(defn- manifest-from-options [options]
  (let [options (or (:types options) mt/type-names)
        vers (u/getenv "SFDC_METADATA_API_VERSION" "51.0")
        content (concat (map types-tag options)
                        [{:tag :version :content [vers]}])]
    {:tag :Package :attrs
     {:xmlns (u/getenv
              "SFDC_METADATA_SCHEMA_URL"
              "http://soap.sforce.com/2006/04/metadata")}
     :content content}))

(defn write-manifest!
  ([options root-path]
   (Util/maybeCreateDirectories root-path)
   (let [xml (manifest-from-options options)]
     (with-open [out-file (java.io.FileWriter.
                           (str root-path path-sep manifest-file-name))]
       (xml/emit xml out-file))))
  ([options]
   (write-manifest! options ".")))

(defn prepare-deploy-package []
  (when-let [journal (seq (read-journal-entries))]
    (write-manifest! (keys journal) deploy-root-path)
    (doseq [vs (vals journal)]
      (doseq [src vs]
        (let [parts (s/split src path-sep-re-pattern)
              folder (s/join
                      path-sep
                      (conj (drop 1 (drop-last parts)) deploy-root-path))
              dest (str folder path-sep (last parts))]
          (Util/maybeCreateDirectories folder)
          (Util/copyOrReplaceFile src dest))))
    (Zip/zipFolder deploy-root-path)))

(defn finalize-deploy [deploy-package-name]
  (and (Util/forceDeleteDirectory deploy-root-path)
       (Util/deleteFile deploy-package-name)
       (Util/deleteFile journal-file)))

(defn init-local-store [package-file dest-dir]
  (Zip/unzip package-file dest-dir)
  (Util/deleteFile package-file)
  (Util/deleteFile manifest-file-name)
  (when (and (git/add dest-dir [storage-root])
             (git/commit dest-dir "first commit"))
    true))
