(ns fractl.git
  "A wrapper for git shell commands"
  (:require [fractl.util :as u])
  (:use [clj-jgit.porcelain]))

(defn status [repo-dir]
  (git-status (load-repo repo-dir)))

(defn add-all-files [repo-dir]
  (try
    (git-add (load-repo repo-dir) ".")
    (catch Exception e
      (println e))))

(defn commit [repo-dir commit-message]
   (try
     (git-commit (load-repo repo-dir) commit-message :all? true)
       (catch Exception e
         (println e))))

(defn pull [repo-dir]
  (try
    (git-pull (load-repo repo-dir))
    (catch Exception e
      (println e))))

(defn commit-and-push
  "Commits and push.
  If branch given then, checkout branch -> commit and push."
  [repo-dir branch-name commit-message]
  (let [git-dir (load-repo repo-dir)]
    (if (contains? (into #{} (git-branch-list git-dir)) branch-name)
      (do (git-checkout branch-name)
          (commit git-dir commit-message))
      (do (git-branch-create branch-name)
          (git-checkout branch-name)
          (commit git-dir commit-message)))
    (git-push git-dir)))
