(ns fractl.inference.embeddings.internal.registry)

(def ^:private registry (atom {}))

(defn register-db [db-name connector]
  (swap! registry assoc db-name connector)
  db-name)

(defn fetch-db-connector [db-name]
  (get @registry db-name))
