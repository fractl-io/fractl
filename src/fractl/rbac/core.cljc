(ns fractl.rbac.core
  (:require [clojure.core.memoize :as mem]
            [fractl.rbac.model]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.lang.internal :as li]))

(def ^:private superuser (atom nil))

(def ^:private find-su-event
  (cn/make-instance
   {:Kernel.RBAC/FindUser
    {:Name "superuser"}}))

(defn- lookup-superuser []
  (when-let [r (ev/safe-eval find-su-event)]
    (first r)))

(defn- upsert-superuser []
  (let [evt (cn/make-instance
             {:Kernel.RBAC/Upsert_User
              {:Instance
               {:Kernel.RBAC/User
                {:Name "superuser"}}}})]
    (first
     (ev/safe-eval evt))))

(defn init []
  (let [su (or (lookup-superuser)
               (upsert-superuser))]
    (reset! superuser su)
    true))

(defn superuser? [user]
  (cn/same-instance? user @superuser))

(defn superuser-id? [id]
  (= id (cn/id-attr @superuser)))

(def ^:private cache-threshold 1000)

(def privileges
  (mem/lu
   (fn [user-name]
     (when-let [rs (ev/safe-eval-internal
                    {:Kernel.RBAC/FindRoleAssignments
                     {:Assignee user-name}})]
       (let [ps (ev/safe-eval-internal
                 {:Kernel.RBAC/FindPrivilegeAssignments
                  {:RoleNames (mapv :Role rs)}})]
         (ev/safe-eval-internal
          {:Kernel.RBAC/FindPrivileges
           {:Names (mapv :Privilege ps)}}))))
   :lu/threshold cache-threshold))

(defn- has-priv? [action user-name resource]
  (let [r (if (keyword? resource)
            resource
            (li/make-path resource))]
    (seq
     (filter
      (fn [p]
        (and (some #{r} (:Resource p))
             (some #{action} (:Actions p))))
      (privileges user-name)))))

(def can-read? (partial has-priv? :read))
(def can-upsert? (partial has-priv? :upsert))
(def can-delete? (partial has-priv? :delete))
(def can-eval? (partial has-priv? :eval))
