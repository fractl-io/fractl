(ns fractl.lang.postproc
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]))

(def ^:private postproc-events (u/make-cell []))

(def ^:private inited-roles (u/make-cell #{}))
(def ^:private allow-all [:create :update :delete :read])

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

(defn- merge-rbac-specs [rec1 rec2]
  ;; NOTE: rbac resolution for relationship - first cut
  (let [[r1 r2] [(:rbac (cn/fetch-meta rec1))
                 (:rbac (cn/fetch-meta rec2))]]
    (or r2 r1)))

(defn- rbac-spec-for-relationship [relname]
  (if-let [[e1 e2] (cn/relationship-nodes relname)]
    (merge-rbac-specs e1 e2)
    (u/throw-ex (str "failed to fetch nodes for " relname))))

(defn- rbac-spec-of-parent [recname]
  (when-let [ps (cn/containing-parents recname)]
    (let [[_ _ p] (first ps)]
      (or (:rbac (cn/fetch-meta p))
          (rbac-spec-of-parent p)))))

(defn- intern-rbac [evaluator recname spec]
  (let [pats (vec (su/nonils (flatten (rbac-patterns recname spec))))
        [c n] (li/split-path recname)
        event-name (li/make-path c (keyword (str (name n) "_reg_rbac")))]
    (cn/intern-event event-name {})
    (cn/register-dataflow event-name pats)
    (evaluator {event-name {}})))

(defn rbac [recname rel spec]
  (let [cont (fn [evaluator]
               (cond
                 rel (when-let [spec (rbac-spec-for-relationship recname)]
                       (intern-rbac evaluator recname spec))
                 (not spec) (when-let [spec (rbac-spec-of-parent recname)]
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
