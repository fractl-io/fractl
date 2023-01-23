(ns fractl.rbac.core
  (:require [clojure.core.memoize :as mem]
            [fractl.rbac.model]
            [fractl.global-state :as gs]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.lang.internal :as li]
            [fractl.evaluator.intercept.internal :as ii]))

(defn get-superuser-email []
  (or (get-in (gs/get-app-config) [:authentication :superuser-email])
      "superuser@superuser.com"))

(def ^:private superuser (atom nil))

(defn- find-su-event []
  (cn/make-instance
   {:Kernel.Identity/FindUser
    {:Email (get-superuser-email)}}))

(defn- lookup-superuser []
  (when-let [r (ev/safe-eval (find-su-event))]
    (first r)))

(defn- upsert-superuser [pswd]
  (let [evt (cn/make-instance
             {:Kernel.Identity/Upsert_User
              {:Instance
               {:Kernel.Identity/User
                (merge {:Email (get-superuser-email)}
                       (when pswd
                         {:Password pswd}))}}})]
    (first
     (ev/safe-eval evt))))

(defn init
  ([config]
   (let [su (or (lookup-superuser)
                (upsert-superuser (:superuser-password config)))]
     (reset! superuser su)
     true))
  ([] (init nil)))

(defn superuser? [user]
  (cn/same-instance? user @superuser))

(defn superuser-email? [email]
  (= email (:Email @superuser)))

(defn- find-privileges [role-names]
  (ev/safe-eval-internal
   {:Kernel.RBAC/FindPrivilegeAssignments
    {:RoleNames role-names}}))

(defn- find-child-role-names [role-names]
  (loop [rns role-names, roles []]
    (if-let [r (first rns)]
      (if-let [child-roles
               (first
                (ev/safe-eval-internal
                 {:Kernel.RBAC/FindChildren
                  {:Parent r}}))]
        (let [names (mapv :Name child-roles)]
          (recur
           (rest rns)
           (concat
            names
            (find-child-role-names names))))
        (recur (rest rns) roles))
      roles)))

(def ^:private find-child-privileges
  (comp find-privileges find-child-role-names))

(def ^:private cache-threshold 1000)

(def privileges
  (mem/lu
   (fn [user-name]
     (when-let [rs (ev/safe-eval-internal
                    {:Kernel.RBAC/FindRoleAssignments
                     {:Assignee user-name}})]
       (let [role-names (mapv :Role rs)
             ps0 (find-privileges role-names)
             ps1 (find-child-privileges role-names)
             ps (set (concat ps0 ps1))]
         (ev/safe-eval-internal
          {:Kernel.RBAC/FindPrivileges
           {:Names (mapv :Privilege ps)}}))))
   :lu/threshold cache-threshold))

(defn force-reload-privileges! []
  (mem/memo-clear! privileges))

(defn- has-priv-on-resource? [resource priv-resource]
  (if (or (= :* priv-resource)
          (= resource priv-resource))
    true
    (let [[rc rn :as r] (li/split-path resource)
          [prc prn :as pr] (li/split-path priv-resource)]
      (cond
        (= r pr) true
        (and (= rc prc)
             (= prn :*)) true
        :else false))))

(defn- filter-privs [privs action ignore-refs resource]
  (seq
   (filter
    (fn [p]
      (and (some (partial has-priv-on-resource? resource)
                 (mapv #(if ignore-refs (li/root-path %) %) (:Resource p)))
           (some #{action :*} (:Actions p))))
    privs)))

(defn- has-priv? [action userid arg]
  ;; Assumes - (not (superuser-email? userid))
  (let [resource (:data arg)
        privs (privileges userid)
        predic (partial filter-privs privs action (:ignore-refs arg))]
    (if (ii/attribute-ref? resource)
      (let [rp (li/root-path resource)]
        (or (predic rp)
            (predic resource)))
      (predic resource))))

(def can-read? (partial has-priv? :read))
(def can-upsert? (partial has-priv? :upsert))
(def can-delete? (partial has-priv? :delete))
(def can-eval? (partial has-priv? :eval))

(defn- fetch-instance-info [obj]
  (when-let [inst (cond
                    (cn/entity-instance? obj) obj
                    (and (seqable? obj)
                         (cn/entity-instance? (first obj)))
                    (first obj))]
    [(cn/instance-type inst) (cn/idval inst)]))

(defn- find-instance-privileges [instance-type instance-id]
  (seq
   (ev/safe-eval-internal
    (cn/make-instance
     {:Kernel.RBAC/FindInstancePrivileges
      {:Resource (if (keyword? instance-type)
                   instance-type
                   (li/make-path instance-type))
       :ResourceId instance-id}}))))

(defn- filter-instance-privilege? [opr inst-privs]
  (every? (fn [p] (some #{opr} (:Filter p))) inst-privs))

(defn check-instance-privilege
  "Load instance-level privileges for resource.
   If none are defined, return :continue to execute the rest of the rbac-algo.
   If instance-level privilege is set for the user and opr is in :Actions,
   return :allow, otherwise return :block.
   If instance-level privilege is not set for the user, but opr is in :Filter, return :continue,
   otherwise return :block"
  [userid opr resource]
  (or
   (when-let [[instance-type instance-id] (fetch-instance-info resource)]
     (when-let [inst-privs (find-instance-privileges instance-type instance-id)]
       (let [inst-priv-for-user (first (filter #(= userid (:Assignee %)) inst-privs))]
         (if inst-priv-for-user
           (if (some #{opr} (:Actions inst-priv-for-user))
             :allow
             :block)
           (if (filter-instance-privilege? opr inst-privs)
             :continue
             :block)))))
   :continue))

(defn instance-privilege-assignment-object? [obj]
  (cn/instance-of? :Kernel.RBAC/InstancePrivilegeAssignment obj))

(def instance-privilege-assignment-resource :Resource)
(def instance-privilege-assignment-resource-id :ResourceId)
