(ns fractl.evaluator.intercept.rbac
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.store :as store]
            [fractl.env :as env]
            [fractl.lang.internal :as li]
            [fractl.rbac.core :as rbac]
            [fractl.evaluator.intercept.internal :as ii]))

(defn- has-priv? [rbac-predic user data]
  (if (ii/apply-to-attribute? data)
    true ;; TODO: implement attribute-level checks
    (let [p (partial rbac-predic user)
          rec-name
          (cond
            (keyword? data) data
            (cn/an-instance? data) (cn/instance-type data)
            (li/parsed-path? data) (li/make-path data)
            :else (u/throw-ex (str "invalid argument for rbac interceptor - " data)))]
      (if rec-name
        (p rec-name)
        (let [rs (set (map cn/instance-type data))]
          (su/all-true? (map #(p %) rs)))))))

(def ^:private apply-upsert-rules (partial has-priv? rbac/can-upsert?))
(def ^:private apply-read-rules (partial has-priv? rbac/can-read?))
(def ^:private apply-delete-rules (partial has-priv? rbac/can-delete?))
(def ^:private apply-eval-rules (partial has-priv? rbac/can-eval?))

(def ^:private actions
  {:upsert apply-upsert-rules
   :read apply-read-rules
   :delete apply-delete-rules
   :eval apply-eval-rules})

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

(defn- run [env opr arg]
  (if-let [data (ii/data-input arg)]
    (let [is-delete (= :delete opr)
          user (cn/event-context-user (ii/event arg))
          inst (if is-delete (second data) data)
          check-on (if is-delete (first data) data)]
      (when (or (and (ii/has-instance-meta? arg)
                     (user-is-owner? user env inst))
                ((opr actions) user check-on))
        arg))
    arg))

(defn make [_] ; config is not used
  (ii/make-interceptor :rbac run))
