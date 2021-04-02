(ns fractl.git
  "A wrapper for git shell commands"
  (:require [fractl.util :as u])
  (:use [clojure.java.shell :only [sh]]))

(defn- sh! [& args]
  (let [{exit :exit out :out} (apply sh args)]
    (if (zero? exit)
      true
      (u/throw-ex (str (first args) " failed - " out)))))

(defn commit [repo-dir commit-message]
  (try
    (sh! "git" "commit" "-a" "-m" commit-message :dir repo-dir)
    (catch Exception ex
      (println ex))))

(defn push
  ([repo-dir branch-name]
   (if-let [url (u/getenv "SFDC_GIT_URL" false)]
     (sh! "git" "push" url "--all"
          :dir repo-dir)
     (sh! "git" "push" "origin"
          (or branch-name "main")
          :dir repo-dir)))
  ([repo-dir]
   (push repo-dir nil)))

(defn commit-and-push
  ([repo-dir commit-message branch-name]
   (commit repo-dir commit-message)
   (push repo-dir branch-name))
  ([repo-dir commit-message]
   (commit-and-push repo-dir commit-message nil)))

(defn pull
  ([repo-dir branch-name]
   (sh! "git" "pull" "origin" branch-name :dir repo-dir))
  ([repo-dir]
   (pull repo-dir "main")))

(defn add [repo-dir file-names]
  (loop [fns file-names]
    (if-let [f (first fns)]
      (do (sh! "git" "add" f :dir repo-dir)
          (recur (rest fns)))
      file-names)))
