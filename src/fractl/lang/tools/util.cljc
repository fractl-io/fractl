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

(defn- repo-exists? [path repo]
  #?(:clj
     (let [dir (last (s/split repo #"/"))]
       (.isDirectory (io/file (str path u/path-sep dir))))))

(defn maybe-clone-model [spec path]
  #?(:clj
     (when-let [repo (and (seq spec)
                          (first (s/split spec #" ")))]
       (when-not (repo-exists? path repo)
         (u/exec-in-directory
          path (str "git clone git@github.com:" repo ".git"))))
     :cljs (u/throw-ex "git clone not supported"))
  spec)
