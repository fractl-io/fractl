(ns fractl.resolver.git
  (:require [fractl.resolver.core :as r]
            [fractl.store.sfdc.metadata :as m]
            [fractl.git :as git]))

(defn- git-eval [inst]
  (let [repo-dir (m/metadata-root)]
    (git/add-all repo-dir)
    (git/commit-and-push
     repo-dir
     (str "commit - " (java.util.Date.)))))

(def ^:private resolver-fns
  {:eval {:handler git-eval}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
