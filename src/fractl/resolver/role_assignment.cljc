(ns fractl.resolver.role-assignment
  "Policy management"
  (:require [fractl.lang :as f]
            [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.rbac.role-assignment]))

(defn ra-upsert [inst]
  (let [r (:Role inst)
        a (:Assignee inst)
        aid (:Id a)
        atype (cn/instance-type a)
        evt (cn/make-instance
             {:Kernel.Role/Upsert_Assignment
              {:Role r :AssigneeId aid :AssigneeType atype}})
        result (ev/eval-all-dataflows evt)]
    (when-let [r (ev/ok-result result)]
      (assoc inst :Id (:Id r)))))

(defn- ra-delete [inst]
  (let [id (:Id inst)
        evt (cn/make-instance
             {:Kernel.Role/Delete_Assignment
              {:Id id}})
        result (ev/eval-all-dataflows evt)]
    (when (ev/ok-result result)
      id)))

(def ^:private resolver-fns
  {:upsert {:handler ra-upsert}
   :delete {:handler ra-delete}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
