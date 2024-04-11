(ns fractl.lang.tools.util
  (:require [clojure.string :as s]
            #?(:clj [clojure.java.io :as io])
            [fractl.util :as u]))

(defn get-system-model-paths []
  #?(:clj
     (if-let [paths (u/getenv "FRACTL_MODEL_PATHS")]
       (s/split paths #":")
       ["."])
     :cljs ["."]))

(defn- repo-dir [path n]
  (str path u/path-sep n))

(defn- repo-exists? [paths n]
  #?(:clj
     (some
      #(.isDirectory (io/file (repo-dir % n)))
      paths)))

(defn maybe-clone-model [spec paths]
  (when (seqable? spec)
    #?(:clj
       (when-let [repo (and (seq spec)
                            (first (s/split spec #" ")))]
         (let [[repo branch] (s/split repo #":")
               n (last (s/split repo #"/"))]
           (when-not (repo-exists? paths n)
             (u/exec-in-directory
              (first paths) (str "git clone git@github.com:" repo ".git"))
             (when branch
               (u/exec-in-directory
                (repo-dir (first paths) n)
                (str "git checkout " branch))))))
       :cljs (u/throw-ex "git clone not supported")))
  spec)
