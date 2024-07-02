(ns fractl.inference.service.lib.retriever
  (:require [clojure.edn :as edn]
            [fractl.inference.embeddings.internal.pgvector :as pgvector]))

(def default-docs-limit 10)

(defn retrieve-docs [options]
  (let [{:keys [app-uuid
                embedding
                docs-limit]
         :or {docs-limit default-docs-limit}
         } options
        document-classname (pgvector/get-document-classname app-uuid)]
    (pgvector/find-similar-objects {:classname document-classname
                                    :embedding embedding
                                    :limit docs-limit})))

(def default-tools-limit 50)

(defn retrieve-tools [options]
  (let [{:keys [app-uuid
                embedding
                tools-limit]
         :or {tools-limit default-tools-limit}
         } options
        planner-tool-classname (pgvector/get-planner-classname app-uuid)]
    (->> (pgvector/find-similar-objects {:classname planner-tool-classname
                                         :embedding embedding
                                         :limit     tools-limit})
         (reduce (fn [result each]
                   (let [m (edn/read-string each)]
                     (assoc result (:tool-name m)
                                   (dissoc m :tool-name))))
                 {}))))
