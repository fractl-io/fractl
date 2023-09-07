(ns fractl.lang.rbac
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]))

(def ^:private postproc-events (u/make-cell []))

(def ^:private inited-roles (u/make-cell #{}))
(def ^:private allow-all [:create :update :delete :read])
(def ^:private admin-rbac-spec {:roles ["admin"] :allow :*})

(defn- valid-perm? [s]
  (if (some #{s} allow-all)
    true
    false))

(defn- validate-perms [alw]
  (if (= :* alw)
    allow-all
    (if (and (seq alw) (every? true? (mapv valid-perm? alw)))
      alw
      (u/throw-ex (str "invalid permissions in " alw)))))

(defn- create-roles [roles spec]
  (when (or (not (seq roles)) (not (every? string? roles)))
    (u/throw-ex (str "invalid roles in " spec)))
  (when-let [roles (seq (set/difference roles (set @inited-roles)))]
    (let [r (mapv
             (fn [r] {:Fractl.Kernel.Rbac/Role {:Name r}})
             roles)]
      (u/safe-set inited-roles (set/union roles @inited-roles))
      r)))

(defn- rbac-patterns [recname spec]
  (let [[c n] (li/split-path recname)]
    (mapv
     (fn [{roles :roles allow :allow}]
       [(create-roles (set roles) spec)
        (let [allow (validate-perms allow)
              pname (str "priv_" (name c) "_" (name n)
                         "_" (s/join "_" roles))]
          (concat
           [{:Fractl.Kernel.Rbac/Privilege
             {:Name pname
              :Actions [:q# allow]
              :Resource [:q# [recname]]}}]
           (mapv
            (fn [r]
              {:Fractl.Kernel.Rbac/PrivilegeAssignment
               {:Role r :Privilege pname}})
            roles)))])
     spec)))

(defn- filter-rbac-by-perms [p r]
  (filter (fn [{allow :allow}]
            (or (= allow :*)
                (some p allow)))
          r))

(def ^:private filter-rd (partial filter-rbac-by-perms #{:read}))
(def ^:private filter-upcr (partial filter-rbac-by-perms #{:update :create}))

(defn- merge-reads-with-writes [r1 r2]
  (let [rr1 (filter-upcr r1)
        rr2 (filter-rd r2)]
    (concat rr1 rr2)))

(defn- merge-read-writes [r1 r2]
  (concat (filter-upcr r1)
          (filter-upcr r2)))

(defn- merge-rbac-specs [reltype rec1 rec2]
  (let [[r1 r2] [(:rbac (cn/fetch-meta rec1))
                 (:rbac (cn/fetch-meta rec2))]]
    (case reltype
      :contains (merge-reads-with-writes r1 r2)
      :between (merge-read-writes r1 r2))))

(defn- rbac-spec-for-relationship [relname reltype]
  (if-let [[e1 e2] (cn/relationship-nodes relname)]
    (merge-rbac-specs reltype e1 e2)
    (u/throw-ex (str "failed to fetch nodes for " relname))))

(defn- rbac-spec-of-parent [recname]
  (when-let [ps (cn/containing-parents recname)]
    (let [[_ _ p] (first ps)]
      (or (:rbac (cn/fetch-meta p))
          (when-not (= recname p) (rbac-spec-of-parent p))))))

(defn- conj-admin [spec]
  (if (some #{admin-rbac-spec} spec)
    spec
    (conj spec admin-rbac-spec)))

(def ^:private derived-rbac-tag :derived-rbac)
(def ^:private derived-rbac? derived-rbac-tag)

(def ^:private rbac-assign-events (atom {}))

(defn- register-derived-rbac-assign-event [evaluator recname rolename]
  (let [[c n] (li/split-path recname)
        event-name (li/make-path c (keyword (str (name n) "_DerivedRoleAssignment")))
        aname :Owners]
    (cn/intern-event event-name {aname :Fractl.Kernel.Lang/Any})
    (cn/register-dataflow
     event-name [[:for-each (li/make-ref event-name aname)
                  {:Fractl.Kernel.Rbac/RoleAssignment
                   {:Role rolename :Assignee :%}}]])
    (swap! rbac-assign-events assoc (li/make-path recname) #(evaluator
                                                             {event-name
                                                              {aname %}}))
    event-name))

(defn- ensure-success [df-result]
  (cond
    (map? df-result)
    (if (= :ok (:status df-result))
      df-result
      (u/throw-ex (str "Internal rbac-assignment failed - " (:message df-result))))

    (seqable? df-result) (ensure-success (first df-result))

    :else (u/throw-ex (str "Internal rbac-assignment failed with invalid result - " df-result))))

(defn derived-rbac-assign [recname users]
  (when-let [f (get @rbac-assign-events recname)]
    (ensure-success (f users))))

(defn deregister-derived-rbac-assign-event [recname]
  (swap! rbac-assign-events dissoc (li/make-path recname))
  recname)

(defn- make-derived-role [recname target-recname]
  (let [[c n] (li/split-path recname)
        [_ n2] (li/split-path target-recname)]
    (str (name c) "_" (name n) "_" (name n2) "__derived_role")))

(declare intern-rbac)

(defn- intern-derived-rbac [evaluator recname spec]
  (doseq [s (derived-rbac-tag spec)]
    (let [target-rec (second s)
          rolename (make-derived-role recname target-rec)
          perms (first s)]
      (and (intern-rbac evaluator target-rec [{:roles [rolename]
                                               :allow (if (keyword? perms)
                                                        [perms]
                                                        perms)}])
           (register-derived-rbac-assign-event evaluator recname rolename))))
  (intern-rbac evaluator recname (:spec spec)))

(defn- intern-rbac [evaluator recname spec]
  (if (derived-rbac? spec)
    (intern-derived-rbac evaluator recname spec)
    (let [spec (conj-admin spec)
          pats (vec (su/nonils (flatten (rbac-patterns recname spec))))
          [c n] (li/split-path recname)
          event-name (li/make-path c (keyword (str (name n) "_reg_rbac")))]
      (cn/intern-event event-name {})
      (cn/register-dataflow event-name pats)
      (evaluator {event-name {}}))))

(defn- owners [rel spec]
  (case rel
    :between (li/owner spec)
    false))

(defn- cleanup-spec [recname rel spec]
  (if (map? spec)
    (if-let [spec (:allow-owners spec)]
      (when-let [rules (seq (filter #(keyword? (second %)) spec))]
        {derived-rbac-tag rules :spec (rbac-spec-for-relationship recname rel)})
      (if-let [owner (owners rel spec)]
        (or (:rbac (cn/fetch-meta owner)) (rbac-spec-of-parent owner))
        (:spec spec)))
    spec))

(defn rbac [recname rel spec]
  (let [cont (fn [evaluator]
               (cond
                 rel (when-let [spec (or (cleanup-spec recname rel spec)
                                         (rbac-spec-for-relationship recname rel))]
                       (intern-rbac evaluator recname spec))
                 (not spec) (let [spec (rbac-spec-of-parent recname)]
                              (intern-rbac evaluator recname spec))
                 :else (intern-rbac evaluator recname spec)))]
    (u/safe-set postproc-events (conj @postproc-events cont))
    recname))

(defn eval-events [evaluator]
  (su/nonils
   (mapv #(% evaluator) @postproc-events)))

(defn reset-events! [] (u/safe-set postproc-events []))

(defn- ok? [r]
  (cond
    (map? r) (= :ok (:status r))
    (seqable? r) (ok? (first r))
    :else false))

(defn finalize-events [evaluator]
  (let [rs (eval-events evaluator)]
    (doseq [r rs]
      (when-not (ok? r)
        (u/throw-ex (str "post-process event failed - " r))))
    (reset-events!)
    rs))
