(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
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

(defn- clj-io [model-name]
  (let [prefix (str cljout u/path-sep model-name u/path-sep)]
    [#(read-string (slurp (str prefix %)))
     #(pprint/pprint %2 (io/writer (str prefix %1)))]))

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

(defn- copy-component [reader writer model-name component]
  (if-let [component-decl (find-component-declaration component)]
    (let [component-name (second component-decl)
          component-spec (nth component-decl 2)
          cns-name (symbol (s/lower-case (name component-name)))
          ns-name (symbol (str model-name ".model." cns-name))
          clj-imports (:clj-import component-spec)
          ns-decl `(~(symbol "ns") ~ns-name
                    ~@(if (= 'quote (first clj-imports))
                        (second clj-imports)
                        clj-imports)
                    (:use [fractl.lang]))]
      (concat [ns-decl] component))
    ;; TODO: 1. Collect all local-names (def, defn and defn-), walk each expression in
    ;;          component and append the ns to those names.
    ;;       2. Write the final component to a .clj file in the src directory.
    ;;          Preserve quotes (by using '').
    (u/throw-ex "no component declaration found")))

(defn- copy-model [reader writer model]
  )

(defn- build-clj-project [model-name model-root model components]
  (if (create-clj-project model-name)
    (let [[rd wr] (clj-io model-name)]
      (when-let [spec (update-project-spec model (rd "project.clj"))]
        (wr "project.clj" spec))
      (mapv (partial copy-component rd wr model-name) components)
      (copy-model rd wr model))
    (log/error (str "failed to create clj project for " model-name))))

(defn build-model
  ([model-paths model-name]
   (let [[model model-root] (loader/read-model model-paths model-name)
         components (loader/read-components-from-model model model-root)]
     (FileUtils/deleteDirectory cljout-file)
     (if (.mkdir cljout-file)
       (build-clj-project model-name model-root model components)
       (u/throw-ex (str "failed to create project directory - " cljout)))))
  ([model-name]
   (build-model (get-system-model-paths) model-name)))
