(ns agentlang.store.migration
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.component :as cn]
            [agentlang.store :as store]))

(defn- report-progress [obj]
  (if-let [cn (:component obj)]
    (println "migrating component" cn)
    (log/info (str "executing command " (:command obj)))))

(declare migrate)

(defn- maybe-migrate-kernel [store config]
  (if-let [config (:kernel config)]
    (migrate store :Agentlang config)
    :Agentlang))

(defn migrate [store model-name config]
  (when (maybe-migrate-kernel store config)
    (let [model-spec (cn/fetch-model model-name)]
      (when-not model-spec
        (u/throw-ex (str "model " model-name " not loaded")))
      (when (store/init-all-schema store)
        (let [from-vers (:from config)
              to-vers (:version model-spec)
              components (:components model-spec)]
          (when (store/execute-migration store report-progress from-vers to-vers components)
            (println "done")
            model-name))))))
