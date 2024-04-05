(ns fractl.graphql.core
  (:require [com.walmartlabs.lacinia.schema :as schema]
            [fractl.graphql.resolvers :as resolvers]
            [com.walmartlabs.lacinia.util :as util]
            [clojure.java.io :as io]
            [com.walmartlabs.lacinia.util :as util]
            [com.walmartlabs.lacinia.schema :as schema]
            [clojure.edn :as edn]
            [fractl.graphql.generator :as gg]
            [clojure.pprint :as pprint]))

(def default-graphql-schema-path "resources/graphql/graphql-schema.edn")

(defn save-schema
  ([schema-edn]
   (save-schema schema-edn default-graphql-schema-path))
  ([schema-edn path]
   ;; ensure the directory exists
   (io/make-parents (io/file path))
   (with-open [w (io/writer path)]
     (pprint/pprint schema-edn w))))

(defn load-graphql-schema
  [component-name schema-info]
  (let [graphql-schema (gg/generate-graphql-schema component-name schema-info)
        compiled-schema (-> graphql-schema
                            (util/inject-resolvers (resolvers/resolver-map component-name schema-info))
                            schema/compile)]
    (save-schema graphql-schema)
    compiled-schema))