(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.lang.tools.loader :as loader])
  (:import [java.io File]
           [org.apache.commons.io FileUtils]
           [org.apache.commons.exec CommandLine Executor DefaultExecutor]))

(defn- get-system-model-paths []
  (if-let [paths (System/getenv "FRACTL_MODEL_PATHS")]
    (s/split paths #":")
    ["."]))

(def cljout "cljout")
(def ^:private cljout-file (File. cljout))

(defn- project-dir [model-name]
  (str cljout u/path-sep model-name u/path-sep))

(defn- clj-io [model-name]
  (let [prefix (project-dir model-name)]
    [#(read-string (slurp (str prefix %)))
     (fn [file-name contents & options]
       (let [f (File. (str prefix file-name))
             write-each (first options)]
         (FileUtils/createParentDirectories f)
         (with-open [w (io/writer f)]
           (if write-each
             (doseq [exp contents]
               (pprint/pprint exp w))
             (pprint/pprint contents w)))))]))

(defn- create-clj-project [model-name]
  (let [cmd (str "lein new fractl-model " model-name)
        ^CommandLine cmd-line (CommandLine/parse cmd)
        ^Executor executor (DefaultExecutor.)]
    (.setWorkingDirectory executor cljout-file)
    (zero? (.execute executor cmd-line))))

(defn- update-project-spec [model project-spec]
  (when-let [deps (:clj-dependencies model)]
    (loop [spec project-spec, final-spec []]
      (if-let [s (first spec)]
        (if (= :dependencies s)
          (recur (rest (rest spec))
                 (conj
                  final-spec :dependencies
                  (vec (concat (second spec) deps))))
          (recur (rest spec) (conj final-spec s)))
        (seq final-spec)))))

(defn- find-component-declaration [component]
  (let [f (first component)]
    (when (= 'component (first f))
      f)))

(defn- write-component-clj [model-name component-name write component]
  (let [parts (s/split (str component-name) #"\.")
        compname (last parts)
        dirs (butlast parts)
        file-name
        (str
         "src" u/path-sep model-name u/path-sep "model" u/path-sep
         (s/join u/path-sep (concat dirs [(str compname ".clj")])))]
    (write file-name component true)
    component-name))

(defn- var-name [defexp]
  (first (filter symbol? defexp)))

(defn- rewrite-in-decl [ns-name local-defs full-defs-map decl]
  (w/prewalk
   #(if (and (symbol? %)
             (some #{%} local-defs))
      (get full-defs-map % %)
      %)
   decl))

(def ^:private clj-defs #{'def 'defn 'defn-})
(def ^:private fractl-defs #{'entity 'dataflow 'event 'record})

(defn- update-local-defs [ns-name component]
  (let [local-defs (set
                    (mapv
                     #(var-name (rest %))
                     (filter #(and (seqable? %)
                                   (some #{(first %)} clj-defs))
                             component)))
        updated-defs (into {} (mapv (fn [d] [d (symbol (str ns-name "/" d))]) local-defs))
        rw (partial rewrite-in-decl ns-name local-defs updated-defs)]
    (mapv
     #(if (and (seqable? %)
               (some #{(first %)} fractl-defs))
        (rw %)
        %)
     component)))

(defn- copy-component [write model-name component]
  (if-let [component-decl (find-component-declaration component)]
    (let [component-name (second component-decl)
          component-spec (when (> (count component-decl) 2)
                           (nth component-decl 2))
          cns-name (symbol (s/lower-case (name component-name)))
          ns-name (symbol (str model-name ".model." cns-name))
          clj-imports (:clj-import component-spec)
          ns-decl `(~(symbol "ns") ~ns-name
                    ~@(if (= 'quote (first clj-imports))
                        (second clj-imports)
                        clj-imports)
                    (:use [fractl.lang]))]
      (write-component-clj
       model-name cns-name write
       (concat
        [ns-decl]
        (update-local-defs ns-name component))))
    (u/throw-ex "no component declaration found")))

(defn- write-model-clj [write model-name component-names model]
  (let [root-ns-name (symbol (str model-name ".model"))
        req-comp (mapv (fn [c] [(symbol (str root-ns-name "." c))]) component-names)
        ns-decl `(~'ns ~(symbol (str root-ns-name ".model")) (:use ~@req-comp))]
    (write (str "src" u/path-sep model-name u/path-sep "model" u/path-sep "model.clj")
           [ns-decl model] true)))

(defn- build-clj-project [model-name model-root model components]
  (if (create-clj-project model-name)
    (let [[rd wr] (clj-io model-name)]
      (when-let [spec (update-project-spec model (rd "project.clj"))]
        (wr "project.clj" spec))
      (let [cmps (mapv (partial copy-component wr model-name) components)]
        (write-model-clj wr model-name cmps model)
        model-name))
    (log/error (str "failed to create clj project for " model-name))))

(declare install-model)

(defn- install-dependencies! [model-paths deps]
  (doseq [[model-name _ :as d] deps]
    (when-not (install-model model-paths (s/lower-case (name model-name)))
      (u/throw-ex (str "failed to install dependency " d)))))

(defn build-model
  ([model-paths model-name]
   (let [[model model-root :as result] (loader/read-model model-paths model-name)
         components (loader/read-components-from-model model model-root)
         projdir (File. (project-dir model-name))]
     (install-dependencies! model-paths (:dependencies model))
     (FileUtils/deleteDirectory projdir)
     (when (build-clj-project model-name model-root model components)
       result)))
  ([model-name]
   (build-model (get-system-model-paths) model-name)))

(defn install-model [model-paths model-name]
  (when-let [result (build-model model-paths model-name)]
    (let [cmd "lein install"
          ^CommandLine cmd-line (CommandLine/parse cmd)
          ^Executor executor (DefaultExecutor.)
          projdir (project-dir model-name)]
      (.setWorkingDirectory executor (File. projdir))
      (when (zero? (.execute executor cmd-line))
        result))))
