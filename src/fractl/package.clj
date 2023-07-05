(ns fractl.package
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.io :as iou])
  (:import [java.io File]
           [java.util.regex Pattern]
           [fractl.filesystem Util]))

(def ^:private path-split-pat (re-pattern (Pattern/quote iou/path-sep)))
(def ^:private model-resource-root "app")

(defn- project-spec [project-name model]
  `(~(symbol "defproject")
    ~(symbol project-name)
    ~(or (:version model)
         "0.1-SNAPSHOT")

    :main ~(symbol "fractl.core")

    :repositories [["public-github" {:url "git://github.com"}]
                   ["private-github" {:url "git://github.com" :protocol :ssh}]]

    ~@(when-let [deps (:clj-dependencies model)]
        [:dependencies deps])))

(defn- fetch-app-config [model-root-dir launch-config]
  (let [^File file (File. (str model-root-dir iou/path-sep "config.edn"))]
    (if (.exists file)
      (slurp file)
      launch-config)))

(defn- generate-project [model-file project-dir model model-root-dir config]
  (when-not (Util/forceDeleteDirectory project-dir)
    (u/throw-ex (str "failed to delete old project directory - " project-dir)))
  (let [mdir (last (s/split model-root-dir path-split-pat))
        dest-root (str project-dir iou/path-sep model-resource-root)
        mroot (str dest-root iou/path-sep mdir)
        mfile (.getName (File. model-file))]
    (Util/maybeCreateDirectories dest-root)
    (iou/pretty-spit (str project-dir iou/path-sep "config.edn")
                     (assoc (fetch-app-config model-root-dir config)
                            :full-model-path
                            (str "." iou/path-sep model-resource-root
                                 iou/path-sep mdir iou/path-sep mfile)))
    (iou/pretty-spit (str project-dir iou/path-sep "project.clj")
                     (project-spec project-dir model))
    (Util/copyDirectory model-root-dir mroot))
  {:model (:name model) :project-path (str "." iou/path-sep project-dir)})

(defn- normalize-model-name-dir [s]
  (s/replace s #"/" "_"))

(defn- model-name-dir [model]
  (when-let [n (:name model)]
    (let [s (if (keyword? n)
              (subs (str n) 1)
              n)]
      (s/lower-case (normalize-model-name-dir s))))) 

(defn build [model-file [[model model-root-dir] config]]
  (if-let [n (model-name-dir model)]
    (generate-project model-file n model model-root-dir config)
    (u/throw-ex (str "model name is required " - model))))
