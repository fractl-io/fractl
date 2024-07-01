(ns fractl.graphql.core
  (:require [clojure.tools.reader.edn :as edn]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.util :as util]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [fractl.graphql.generator :as gg]
            [fractl.graphql.resolvers :as gr]
            [fractl.global-state :as gs]))

(def default-graphql-schema-path "resources/graphql/graphql-schema.edn")

(defn save-schema
  ([schema-edn]
   (save-schema schema-edn default-graphql-schema-path))
  ([schema-edn path]
   ;; ensure the directory exists
   (io/make-parents (io/file path))
   (with-open [w (io/writer path)]
     (pprint/pprint schema-edn w))))

(defn compile-graphql-schema
  [schema-info contains-graph]
  (let [app-config (gs/get-app-config)
        schema-path (get-in app-config [:graphql :schema-path])
        [graphql-schema entity-metas]
        (if (and schema-path (.exists (io/file schema-path)))
          (let [schema-edn (edn/read-string (slurp schema-path))]
            [schema-edn (second (gg/generate-graphql-schema schema-info))])
          (gg/generate-graphql-schema schema-info))
        graphql-resolver-map (gr/generate-resolver-map schema-info contains-graph)
        compiled-schema (-> graphql-schema
                            (util/inject-resolvers graphql-resolver-map)
                            schema/compile)]
    [graphql-schema compiled-schema entity-metas]))