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
            [fractl.rbac.core :as rbac]
            [fractl.global-state :as gs]
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

(defn- make-read-output-arg [old-output-data new-insts]
  (if (contains-env? old-output-data)
    (concat [new-insts] (rest old-output-data))
    new-insts))

(defn- apply-read-attribute-rules [user rslt arg]
  (let [inst (first rslt)
        attr-names (keys (cn/instance-attributes inst))
        inst-type (cn/instance-type inst)
        res-names (mapv (partial ii/wrap-attribute inst-type) attr-names)
        readable-attrs (or (seq
                            (mapv
                             #(first (:refs (li/path-parts %)))
                             (filter #(apply-read-rules user {:data %}) res-names)))
                             attr-names)
        hidden-attrs (set/difference (set attr-names) (set readable-attrs))
        new-insts (mapv #(apply dissoc % hidden-attrs) rslt)]
    (ii/assoc-data-output
     arg (make-read-output-arg (ii/data-output arg) new-insts))))

(defn- user-is-owner?
  ([user env data]
   (when (cn/entity-instance? data)
     (user-is-owner?
      user env
      (cn/instance-type data)
      (cn/idval data))))
  ([user env inst-type id]
   (when (and inst-type id)
     (when-let [meta (store/lookup-by-id
                      (env/get-store env)
                      (cn/meta-entity-name inst-type)
                      cn/meta-entity-id (str id))]
       (= (cn/instance-meta-owner meta) user)))))

(defn- first-instance [data]
  (cond
    (keyword? data) data
    (map? data) data
    (and (seqable? data) (cn/an-instance? (first data)))
    (first data)
    :else data))

(defn- find-parent [env inst]
  (let [r (cn/find-contained-relationship (cn/instance-type inst))
        pn (when r (first (mt/contains (cn/fetch-meta r))))]
    (and pn (env/lookup env pn))))

(defn- parent-of? [env user inst]
  (when inst
    (or (user-is-owner? user env (cn/instance-type inst) (cn/idval inst))
        (parent-of? user env (find-parent env inst)))))

(defn- check-inherited-instance-privilege [user opr instance]
  (or
   (when (cn/entity-instance? instance)
     (let [entity-name (cn/instance-type instance)]
       (loop [rels (seq (cn/relationships-with-instance-rbac entity-name))
              result :continue]
         (if-let [r (first rels)]
           ;; TODO rg/find-connected-nodes can be optimized for queries on parents.
           ;; (https://github.com/fractl-io/fractl/issues/712)
           (if-let [nodes (seq (rg/find-connected-nodes r entity-name instance))]
             (let [pvs (mapv #(let [p (rbac/check-instance-privilege user opr %)]
                                (if (= :continue p)
                                  (check-inherited-instance-privilege user opr %)
                                  p))
                             nodes)]
               (if (some #{:block} pvs)
                 :block
                 (recur (rest pvs) (some #{:allow} pvs))))
             result)
           result))))
   :continue))

(defn- check-inherited-entity-privilege [user opr instance rbac-check]
  (let [entity-name (cond
                      (keyword? instance)
                      (li/root-path instance)

                      (vector? instance)
                      (li/make-path instance)

                      :else
                      (cn/instance-type instance))]
    (if (cn/entity? entity-name)
      (if-let [rels (seq (cn/relationships-with-entity-rbac entity-name))]
        (if (every? #(let [e1 (cn/containing-parent %)]
                       (or (rbac-check e1)
                           (= :allow (check-inherited-entity-privilege user opr e1 rbac-check))))
                    rels)
          :allow
          :block)
        :continue)
      :continue)))

(defn- call-rbac-continuation [user resource opr r c]
  (case r
    :allow true
    :block false
    :continue (c)))

(defn- apply-privilege-hierarchy-checks [env user opr resource rbac-check continuation]
  (if (cn/entity-instance? resource)
    (or (parent-of? env user (find-parent env resource))
        (call-rbac-continuation user resource opr
         (check-inherited-instance-privilege user opr resource)
         #(call-rbac-continuation user resource opr
           (check-inherited-entity-privilege user opr resource rbac-check)
           continuation)))
    (call-rbac-continuation user resource opr
     (check-inherited-entity-privilege user opr resource rbac-check)
     continuation)))

(defn- check-instance-privilege
  ([env user arg opr resource rbac-check continuation]
   (let [r (if (cn/entity-instance? resource)
             (if (and (some #{opr} #{:update :create :delete :read})
                      (rbac/instance-privilege-assignment-object? resource))
               (and
                (user-is-owner?
                 user env
                 (rbac/instance-privilege-assignment-resource resource)
                 (rbac/instance-privilege-assignment-resource-id resource))
                :allow)
               (rbac/check-instance-privilege user opr resource))
             :continue)]
     (if (= r :block)
       (do (ii/set-user-state-value! arg :blocked-at-instance-level r)
           false)
       (call-rbac-continuation
        user resource opr
        r #(if continuation
             (apply-privilege-hierarchy-checks
              env user opr resource rbac-check continuation)
             true)))))
  ([env user arg opr resource]
   (check-instance-privilege env user arg opr resource nil nil)))

(defn- obj-name? [x]
  (if (map? x)
    false
    (or (keyword? x)
        (and (seqable? x)
             (every? keyword? x)))))

(defn- apply-rbac-for-user [user env opr arg]
  (if-let [data (ii/data-input arg)]
    (if (or (ii/skip-for-input? data) (= opr :read))
      arg
      (let [is-delete (= :delete opr)
            resource (if is-delete (second data) (first-instance data))
            check-on (if is-delete (first data) resource)
            ign-refs (or is-delete (= :read opr))]
        (when (or (and (ii/has-instance-meta? arg)
                       (user-is-owner? user env resource))
                  (let [check-arg {:data check-on :ignore-refs ign-refs}]
                    (check-instance-privilege
                     env user arg opr resource
                     #(apply-rbac-for-user user env opr (ii/assoc-data-input arg %))
                     #((opr actions) user check-arg))))
          arg)))
    (if-let [data (seq (ii/data-output arg))]
      (cond
        (ii/skip-for-output? data)
        arg

        (= :read opr)
        (let [rslt (extract-read-results data)]
          (if (and (ii/has-instance-meta? arg)
                   (every? (partial user-is-owner? user env) rslt))
            arg
            (when (every? #(check-instance-privilege
                            env user arg opr %
                            (fn [a] (apply-rbac-for-user user env opr (ii/assoc-data-output arg a)))
                            (fn [] ((opr actions) user {:data % :ignore-refs true})))
                          rslt)
              (apply-read-attribute-rules user rslt arg))))

        :else arg)
      arg)))

(defn- check-upsert-on-attributes [env opr user arg]
  (when-let [inst (first-instance (ii/data-input arg))]
    (let [n (cn/instance-type inst)
          idattr (cn/identity-attribute-name n)
          attrs (remove #(= idattr %) (keys (cn/instance-attributes inst)))
          waf (partial ii/wrap-attribute n)]
      (when (every? #(apply-rbac-for-user user env opr (ii/assoc-data-input arg (waf %))) attrs)
        arg))))

(defn- blocked-at-instance-level? [arg]
  (= :block (ii/get-user-state-value arg :blocked-at-instance-level)))

(def ^:private system-events #{[:Fractl.Kernel.Identity :SignUp]
                               [:Fractl.Kernel.Identity :ForgotPassword]
                               [:Fractl.Kernel.Identity :ConfirmForgotPassword]
                               [:Fractl.Kernel.Identity :ConfirmSignUp]})

(defn- system-event? [inst]
  (when-let [t (cn/instance-type inst)]
    (or (cn/an-internal-event? t)
        (some #{(li/split-path t)} system-events))))

(defn- run [env opr arg]
  (let [user (or (cn/event-context-user (ii/event arg))
                 (gs/active-user))]
    (if (or (rbac/superuser-email? user)
            (system-event? (ii/event arg)))
      arg
      (let [is-ups (or (= opr :update) (= opr :create))
            arg (if is-ups (ii/assoc-user-state arg) arg)]
        (or (apply-rbac-for-user user env opr arg)
            (when (and is-ups (not (blocked-at-instance-level? arg)))
              (check-upsert-on-attributes env opr user arg)))))))

(defn make [_] ; config is not used
  (ii/make-interceptor :rbac run))
