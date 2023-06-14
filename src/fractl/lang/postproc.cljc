(ns fractl.lang.defer
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [fractl.lang.internal :as li]
            [fractl.util :as u]))

(def ^:private deferred-rbac (u/make-cell {}))
(def ^:private allow-all [:create :update :delete :read])

(defn- valid-perm? [s]
  (if (some #{s} allow-all)
    true
    false))

(defn- allow [spec]
  (let [alw (:allow spec)]
    (if (= :* alw)
      allow-all
      (let [alw (seq alw)]
        (if (and alw (every? true? (mapv valid-perm? alw)))
          alw
          (u/throw-ex (str "invalid permissions in " spec)))))))

(defn defer-rbac [recname spec]
  (let [rbac @deferred-rbac
        rs (get rbac :roles #{})
        recs (get rbac :records {})
        roles (seq (set (:roles spec)))
        alw (allow spec)]
    (when (or (not roles) (not (every? string? (:roles spec))))
      (u/throw-ex (str "invalid roles in " spec)))
    (let [rbac (assoc rbac :roles (set/union roles rs))]
      (u/safe-set deferred-rbac (assoc rbac :records (assoc recs recname spec)))
      recname)))

(defn create-roles []
  (let [rbac @deferred-rbac]
    (mapv
     (fn [r] {:Fractl.Kernel.Rbac/Role {:Name r}})
     (:roles rbac))))

(defn create-privilege-assignments []
  (let [rbac @deferred-rbac
        recs (keys (:records rbac))]
    (apply
     concat
     (mapv
      (fn [recname]
        (let [spec (recname recs)
              [c n] (li/split-path recname)]
          (mapv
           (fn [{roles :roles allow :allow}]
             (let [pname (str "priv_" (name c) "_" (name n)
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
                 roles))))
           spec)))
      recs))))
