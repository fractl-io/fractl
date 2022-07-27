(ns fractl.resolver.git
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.git :as git]))

(defn- git-eval [inst]
  (let [repo-dir (:Path inst)]
    (git/commit-and-push
     repo-dir
     (str "commit - " (java.util.Date.)))))

(def ^:private resolver-fns
  {:eval {:handler git-eval}})

(defmake :git
  (fn [resolver-name config]
    (r/make-resolver resolver-name resolver-fns)))
