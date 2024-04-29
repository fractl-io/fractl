(ns fractl.graphql.core
  (:require [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.util :as util]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [fractl.graphql.generator :as gg]
            [fractl.graphql.resolvers :as gr]))

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
  (let [[graphql-schema entity-metas] (gg/generate-graphql-schema schema-info)
        graphql-resolver-map (gr/generate-resolver-map schema-info contains-graph)
        compiled-schema (-> graphql-schema
                            (util/inject-resolvers graphql-resolver-map)
                            schema/compile)]
    [graphql-schema compiled-schema entity-metas]))