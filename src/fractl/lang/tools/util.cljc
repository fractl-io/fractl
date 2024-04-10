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

(defn- repo-exists? [paths repo]
  #?(:clj
     (let [dir (last (s/split repo #"/"))]
       (some
        #(.isDirectory (io/file (str % u/path-sep dir)))
        paths))))

(defn maybe-clone-model [spec paths]
  #?(:clj
     (when-let [repo (and (seq spec)
                          (first (s/split spec #" ")))]
       (when-not (repo-exists? paths repo)
         (u/exec-in-directory
          (first paths) (str "git clone git@github.com:" repo ".git"))))
     :cljs (u/throw-ex "git clone not supported"))
  spec)
