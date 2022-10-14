(ns fractl.rbac.core
  (:require [clojure.core.memoize :as mem]
            [fractl.rbac.model]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.lang.internal :as li]))

(def default-superuser-name "superuser")
(def ^:private superuser (atom nil))

(defn- find-su-event []
  (cn/make-instance
   {:Kernel.Identity/FindUser
    {:Name default-superuser-name}}))

(defn- lookup-superuser []
  (when-let [r (ev/safe-eval (find-su-event))]
    (first r)))

(defn- upsert-superuser [pswd]
  (let [evt (cn/make-instance
             {:Kernel.Identity/Upsert_User
              {:Instance
               {:Kernel.Identity/User
                (merge {:Name default-superuser-name}
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

(defn superuser-name? [user-name]
  (= user-name (:Name @superuser)))

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

(defn- has-priv? [action user-name resource]
  (if (superuser-name? user-name)
    true
    (seq
     (filter
      (fn [p]
        (and (some (partial has-priv-on-resource? resource)
                   (:Resource p))
             (some #{action :*} (:Actions p))))
      (privileges user-name)))))

(def can-read? (partial has-priv? :read))
(def can-upsert? (partial has-priv? :upsert))
(def can-delete? (partial has-priv? :delete))
(def can-eval? (partial has-priv? :eval))
