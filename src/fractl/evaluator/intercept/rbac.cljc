(ns fractl.evaluator.intercept.rbac
  (:require [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.store :as store]
            [fractl.env :as env]
            [fractl.lang.internal :as li]
            [fractl.rbac.core :as rbac]
            [fractl.evaluator.intercept.internal :as ii]))

(defn- has-priv? [rbac-predic user arg]
  (let [data (:data arg)
        p (partial rbac-predic user)
        rec-name
        (cond
          (keyword? data) data

          (cn/an-instance? data)
          (cn/instance-type data)

          (li/parsed-path? data)
          (li/make-path data)

          :else
          (u/throw-ex (str "invalid argument for rbac interceptor - " data)))]
    (if rec-name
      (p (assoc arg :data rec-name))
      (let [rs (set (map cn/instance-type data))]
        (su/all-true? (map #(p (assoc arg :data %)) rs))))))

(def ^:private apply-upsert-rules (partial has-priv? rbac/can-upsert?))
(def ^:private apply-read-rules (partial has-priv? rbac/can-read?))
(def ^:private apply-delete-rules (partial has-priv? rbac/can-delete?))
(def ^:private apply-eval-rules (partial has-priv? rbac/can-eval?))

(def ^:private actions
  {:upsert apply-upsert-rules
   :read apply-read-rules
   :delete apply-delete-rules
   :eval apply-eval-rules})

(defn- apply-read-attribute-rules [opr user arg]
  (let [data (first (ii/data-output arg))
        inst (first data)
        attr-names (keys (cn/instance-attributes inst))
        inst-type (cn/instance-type inst)
        res-names (mapv (partial ii/wrap-attribute inst-type) attr-names)
        readable-attrs (or (seq
                            (mapv
                             #(first (:refs (li/path-parts %)))
                             (filter #(apply-read-rules user {:data %}) res-names)))
                             attr-names)
        hidden-attrs (set/difference (set attr-names) (set readable-attrs))
        new-insts (mapv #(apply dissoc % hidden-attrs) data)]
    (ii/assoc-data-output arg (concat [new-insts] (rest (ii/data-output arg))))))

(defn- user-is-owner? [user env data]
  (when (cn/entity-instance? data)
    (let [[inst-type id] [(cn/instance-type data)
                          (cn/id-attr data)]]
      (when (and inst-type id)
        (when-let [meta (store/lookup-by-id
                         (env/get-store env)
                         (cn/meta-entity-name inst-type)
                         id)]
          (= (cn/instance-meta-owner meta) user))))))

(defn- first-instance [data]
  (cond
    (keyword? data) data
    (map? data) data
    (and (seqable? data) (cn/an-instance? (first data)))
    (first data)
    :else data))

(defn- run [env opr arg]
  (if-let [data (ii/data-input arg)]
    (let [is-delete (= :delete opr)
          user (cn/event-context-user (ii/event arg))
          resource (if is-delete (second data) (first-instance data))
          check-on (if is-delete (first data) resource)
          ign-refs (and (not is-delete)
                        (not (ii/attribute-ref? resource))
                        (or (= :read opr) (= :upsert opr)))]
      (when (or (and (ii/has-instance-meta? arg)
                     (user-is-owner? user env resource))
                ((opr actions)
                 user
                 {:data check-on
                  :ignore-refs ign-refs}))
        arg))
    (if-let [data (seq (ii/data-output arg))]
      (if (= :read opr)
        (let [user (cn/event-context-user (ii/event arg))]
          (if (and (ii/has-instance-meta? arg)
                   (every? (partial user-is-owner? user env) (first data)))
            arg
            (apply-read-attribute-rules opr user arg)))
        arg)
      arg)))

(defn make [_] ; config is not used
  (ii/make-interceptor :rbac run))
