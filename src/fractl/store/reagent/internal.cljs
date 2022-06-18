(ns fractl.store.reagent.internal
  (:require [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.util :as u]
            [reagent.core :as reagent]))

(def view-tag :DOMView)

(def inst-store (reagent/atom {}))

(defn- store
  []
  @inst-store)

(defn- intern-instance!
  [inst]
  (let [entity-name (cn/instance-type inst)
        parsed-entity (li/split-path entity-name)
        id (cn/id-attr inst)]
    (swap! inst-store assoc-in [parsed-entity id] inst))
  inst)

(defn- validate-references!
  [inst ref-attrs]
  (doseq [[aname scmname] ref-attrs]
    (let [p (cn/find-ref-path scmname)
          component (:component p)
          entity-name (:record p)
          aval (aname inst)
          store-ref [[component entity-name] aval]]
      (when-not (get-in @inst-store store-ref)
        (u/throw-ex (str "Reference not found - " aname ", " p))))))

(defn upsert-instance
  [entity-name inst]
  (let [entity-schema (cn/entity-schema entity-name)
        ref-attrs (cn/ref-attribute-schemas entity-schema)]
    (when (seq ref-attrs)
      (validate-references! inst ref-attrs))
    (intern-instance! inst)))

(defn query-by-id
  [entity-name ids]
  (remove nil? (flatten (map #(get-in @inst-store [entity-name %]) (set ids)))))

(defn query-all
  [entity-name params]
  (vals (get @inst-store entity-name)))

(defn delete-by-id
  [entity-name id]
  (let [parsed-entity (li/split-path entity-name)]
    (u/call-and-set
     inst-store
     #(update-in @inst-store [parsed-entity] dissoc id))
    id))

(defn delete-all
  [entity-name]
  (let [parsed-entity (li/split-path entity-name)]
    (u/call-and-set
     inst-store
     #(dissoc @inst-store parsed-entity))
    entity-name))

(defn get-reference
  [[entity-name id] refs]
  (let [tag (if (= (last refs) view-tag)
              :view-cursor
              :cursor)]
    [tag [entity-name id (last refs)]]))

(defn compile-to-indexed-query [query-pattern]
  (let [entity-name (:from query-pattern)
        where-clause (:where query-pattern)]
    (if (= :* where-clause)
      {:query {:entity entity-name}}
      (let [norm-where-clause (if (= :and (first where-clause))
                                (rest where-clause)
                                [where-clause])]
        {:query {:entity entity-name
                 :filter norm-where-clause}}))))
