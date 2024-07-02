(ns fractl.inference.service.resolver
  (:require [clojure.pprint :as pp]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.inference.service.logic :as logic]))

(defn resolver-create [instance]
  (let [entity-name (:-*-type-*- instance)
        [entity-ns entity-name] (if (keyword? entity-name)
                                  [(keyword (namespace entity-name))
                                   (keyword (name entity-name))]
                                  entity-name)
        entity-data (apply dissoc instance [:type-*-tag-*- :-*-type-*-])]
    (try
      (case entity-name
        :DocChunk (logic/post-doc-chunk instance)
        :PlannerTool (logic/post-planner-tool instance)
        :Question (logic/post-app-question instance)
        (log/error (str "Cannot process entity-name: " entity-name)))
      (catch Exception ex
        (log/exception ex)))))

(defn register-resolver []
  (let [ents (seq (cn/entity-names :Inference.Service))]
    (rg/register-resolver-type
      :inference
      (fn [_ _]
        (r/make-resolver
          :inference
          {:create resolver-create})))
    (rg/register-resolver
     {:name :inference
       :type :inference
       :paths ents})))
