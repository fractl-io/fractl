(ns fractl.rbac.core
  (:require [fractl.global-state :as gs]
            [fractl.util :as u]
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
   {:Fractl.Kernel.Identity/FindUser
    {:Email (get-superuser-email)}}))

(defn init
  ([config]
   (let [su (get-superuser-email)]
     (reset! superuser su)
     true))
  ([] (init nil)))

(defn superuser? [user]
  (cn/same-instance? user @superuser))

(defn superuser-email? [email]
  (= email (:Email @superuser)))

(def ^:private find-privileges
  (u/call-with-cache
   (fn [role-names]
     (when (seq role-names)
       (ev/safe-eval-internal
        {:Fractl.Kernel.Rbac/FindPrivilegeAssignments
         {:RoleNames role-names}})))))

(def ^:private role-assignments
  (u/call-with-cache
   (fn [user-name]
     (ev/safe-eval-internal
      {:Fractl.Kernel.Rbac/FindRoleAssignments
       {:Assignee user-name}}))))

(def privileges
  (u/call-with-cache
   (fn [user-name]
     (when-let [rs (role-assignments user-name)]
       (let [role-names (mapv :Role rs)
             ps (find-privileges role-names)
             names (mapv :Privilege ps)]
         (when (seq names)
           (ev/safe-eval-internal
            {:Fractl.Kernel.Rbac/FindPrivileges
             {:Names names}})))))))

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
(def can-create? (partial has-priv? :create))
(def can-update? (partial has-priv? :update))
(def can-delete? (partial has-priv? :delete))
(def can-eval? (partial has-priv? :eval))
