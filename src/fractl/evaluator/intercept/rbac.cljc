(ns fractl.evaluator.intercept.rbac
  (:require [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.store :as store]
            [fractl.env :as env]
            [fractl.meta :as mt]
            [fractl.lang.internal :as li]
            [fractl.lang.relgraph :as rg]
            [fractl.lang.rbac :as lr]
            [fractl.rbac.core :as rbac]
            [fractl.global-state :as gs]
            [fractl.resolver.registry :as rr]
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

(def ^:private apply-create-rules (partial has-priv? rbac/can-create?))
(def ^:private apply-update-rules (partial has-priv? rbac/can-update?))
(def ^:private apply-read-rules (partial has-priv? rbac/can-read?))
(def ^:private apply-delete-rules (partial has-priv? rbac/can-delete?))
(def ^:private apply-eval-rules (partial has-priv? rbac/can-eval?))

(def ^:private actions
  {:update apply-update-rules
   :create apply-create-rules
   :read apply-read-rules
   :delete apply-delete-rules
   :eval apply-eval-rules})

(defn- contains-env? [obj]
  (and (seqable? obj)
       (env/env? (second obj))))

(defn- extract-read-results [obj]
  (if (contains-env? obj)
    (first obj)
    obj))

(defn- set-read-results [obj rslt]
  (if (contains-env? obj)
    (concat [rslt] (rest obj))
    rslt))

(defn- has-instance-privilege? [user opr resource]
  (some #{opr} (cn/instance-privileges-for-user resource user)))

(defn- owner-exclusive? [resource]
  (li/owner-exclusive-crud
   (cn/fetch-meta (if (keyword? resource)
                    resource
                    (cn/instance-type-kw resource)))))

(defn- instance-priv-assignment? [resource]
  (and (cn/an-instance? resource)
       (= (cn/instance-type-kw resource)
          :Fractl.Kernel.Rbac/InstancePrivilegeAssignment)))

(defn- handle-instance-priv [user env opr inst is-system-event]
  (if (or (= opr :create) (= opr :delete))
    (let [entity-name (:Resource inst)
          id (:ResourceId inst)
          store (env/get-store env)
          res (store/lookup-by-id
               store entity-name
               (cn/identity-attribute-name entity-name) id)]
      (when-not res
        (u/throw-ex (str "resource not found - " [entity-name id])))
      (when-not is-system-event
        (when-not (cn/user-is-owner? user res)
          (u/throw-ex (str "only owner can assign instance-privileges - " [entity-name id]))))
      (let [assignee (:Assignee inst)
            actions (when (= opr :create) (:Actions inst))]
        (if (store/update-instances
             store entity-name
             [(if actions
                (cn/assign-instance-privileges res assignee actions)
                (cn/remove-instance-privileges res assignee))])
          inst
          (u/throw-ex (str "failed to assign instance-privileges - " [entity-name id])))))
    inst))

(defn- apply-rbac-checks [user env opr arg resource check-input]
  (if (instance-priv-assignment? resource)
    (when (handle-instance-priv user env opr resource false) arg)
    (let [has-base-priv ((opr actions) user check-input)]
      (if (= :create opr)
        (when has-base-priv arg)
        (let [is-owner (cn/user-is-owner? user resource)
              has-inst-priv (when-not is-owner (has-instance-privilege? user opr resource))]
          (if (or is-owner has-inst-priv)
            arg
            (if has-base-priv
              (case opr
                :read arg
                (:delete :update) (when-not (owner-exclusive? resource) arg)))))))))

(defn- first-instance [data]
  (cond
    (keyword? data) data
    (map? data) data
    (and (seqable? data) (cn/an-instance? (first data)))
    (first data)
    :else data))

(defn- apply-rbac-for-user [user env opr arg]
  (let [check (partial apply-rbac-checks user env opr arg)]
    (if-let [data (ii/data-input arg)]
      (if (or (ii/skip-for-input? data) (= opr :read))
        arg
        (let [is-delete (= :delete opr)
              resource (if is-delete (second data) (first-instance data))
              check-on (if is-delete (first data) resource)
              ign-refs (or is-delete (= :read opr))]
          (check resource {:data check-on :ignore-refs ign-refs})))
      (if-let [data (seq (ii/data-output arg))]
        (if (ii/skip-for-output? data)
          arg
          (if (= opr :read)
            (if-let [rs (seq (extract-read-results data))]
              (when-let [rslt (seq (filter #(check % {:data % :ignore-refs true}) rs))]
                (ii/assoc-data-output arg (set-read-results data rslt)))
              arg)
            arg))
        arg))))

(defn- check-upsert-on-attributes [user env opr arg]
  (when-let [inst (first-instance (ii/data-input arg))]
    (let [n (cn/instance-type inst)
          idattr (cn/identity-attribute-name n)
          attrs (remove #(= idattr %) (keys (cn/instance-attributes inst)))
          waf (partial ii/wrap-attribute n)]
      (when (every? #(apply-rbac-for-user user env opr (ii/assoc-data-input arg (waf %))) attrs)
        arg))))

(defn- maybe-handle-system-objects [user env opr arg]
  (if-let [data (ii/data-input arg)]
    (if (and (or (= opr :create) (= opr :delete))
             (not (ii/skip-for-input? arg)))
      (let [resource (if (= opr :create) (first-instance data) (second data))]
        (if (instance-priv-assignment? resource)
          (when (handle-instance-priv user env opr resource true) arg)
          arg))
      arg)
    arg))

(def ^:private system-events #{[:Fractl.Kernel.Identity :SignUp]
                               [:Fractl.Kernel.Identity :PostSignUp]
                               [:Fractl.Kernel.Identity :ForgotPassword]
                               [:Fractl.Kernel.Identity :ConfirmForgotPassword]
                               [:Fractl.Kernel.Identity :ConfirmSignUp]})

(defn- system-event? [inst]
  (when-let [t (cn/instance-type inst)]
    (or (cn/an-internal-event? t)
        (some #{(li/split-path t)} system-events))))

(def ^:dynamic pass-thru-mode false)

(defn- fire-derived-rbac-events [env opr relname relid]
  (binding [pass-thru-mode true]
    (case opr
      :create
      (when-let [insts (seq (vals (relname (env/relationship-context env))))]
        (let [owners (set (mapv (comp first cn/owners) insts)) ; only the first owner is considered
              resources (vec
                         (filter
                          #(identity (first %))
                          (map
                           #(let [t (cn/instance-type-kw %)
                                  ident (cn/identity-attribute-name t)]
                              [(first (set/difference owners #{(first (cn/owners %))})) t (ident %)])
                           insts)))]
          (lr/derived-rbac-assign relname owners resources relid)))
      :delete (lr/derived-rbac-remove relname relid)))
  relname)

(defn- run [env opr arg]
  (let [user (or (cn/event-context-user (ii/event arg))
                 (gs/active-user))]
    (if pass-thru-mode
      (maybe-handle-system-objects user env opr arg)
      (let [arg (if (or (rbac/superuser-email? user)
                        (system-event? (ii/event arg)))
                  (maybe-handle-system-objects user env opr arg)
                  (let [is-ups (or (= opr :update) (= opr :create))
                        arg (if is-ups (ii/assoc-user-state arg) arg)]
                    (or (apply-rbac-for-user user env opr arg)
                        (when is-ups
                          (check-upsert-on-attributes user env opr arg)))))]
        (case opr
          :create
          (if-let [inst (first-instance (extract-read-results (ii/data-output arg)))]
            (let [n (and (cn/an-instance? inst) (cn/instance-type-kw inst))]
              (if (and n (cn/between-relationship? n))
                (and (fire-derived-rbac-events env opr n ((cn/identity-attribute-name n) inst)) arg)
                arg))
            arg)
          :delete
          (if-let [[n id] (ii/data-output arg)]
            (and (fire-derived-rbac-events env opr n id) arg)
            arg)
          arg)))))

(defn make [_] ; config is not used
  (ii/make-interceptor :rbac run))
