(ns fractl.resolver.role-assignment
  "Policy management"
  (:require [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.rbac.role-assignment]))

(defn ra-upsert [inst]
  (let [r (:Role inst)
        a (:Assignee inst)
        aid (cn/id-attr a)
        atype (cn/instance-type a)
        evt (cn/make-instance
             {:Kernel.Role/Upsert_Assignment
              {:Role r :AssigneeId aid :AssigneeType atype}})
        result (ev/eval-all-dataflows evt)]
    (when-let [r (ev/ok-result result)]
      (assoc inst cn/id-attr (cn/id-attr r)))))

(defn- ra-delete [inst]
  (let [id (cn/id-attr inst)
        evt (cn/make-instance
             {:Kernel.Role/Delete_Assignment
              {cn/id-attr id}})
        result (ev/eval-all-dataflows evt)]
    (when (ev/ok-result result)
      id)))

(def ^:private resolver-fns
  {:upsert {:handler ra-upsert}
   :delete {:handler ra-delete}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
