(ns fractl.resolver.authentication
  (:require [fractl.util :as u]
            [fractl.util.http :as uh]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
            [fractl.auth.core :as auth]))

(def ^:private crdel-fns
  {:user {:create auth/call-upsert-user
          :delete auth/call-delete-user}
   :role {:create auth/create-role
          :delete auth/delete-role}
   :role-assignment {:create auth/add-user-to-role
                     :delete auth/remove-user-from-role}})

(defn- crdel [tag client config inst]
  (let [n (cn/instance-type-kw inst)]
    (case n
      :Fractl.Kernel.Rbac/RoleAssignment
      (and ((tag (:role-assignment crdel-fns))
            (assoc config auth/client-key client
                   :role-name (:Role inst)
                   :username (:Assignee inst)))
           inst)
      :Fractl.Kernel.Rbac/Role
      (and ((tag (:role crdel-fns))
            (assoc config auth/client-key client
                   :role-name (:Name inst)))
           inst)
      ((tag (:user crdel-fns)) client config inst))))

(def ^:private create (partial crdel :create))
(def ^:private delete (partial crdel :delete))

(defmake :authentication
  (fn [resolver-name config]
    (let [config (merge config (uh/get-aws-config))]
      (if-let [client (auth/make-client config)]
        (r/make-resolver
         resolver-name
         {:create {:handler (partial create client config)}
          :update {:handler (partial auth/call-upsert-user client config)}
          :delete {:handler (partial delete client config)}})
        (u/throw-ex (str "failed to create auth-client for " resolver-name))))))
