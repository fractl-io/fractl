(ns fractl.evaluator.intercept.internal)

(def user :user)
(def data :data)

(defn encode-arg [usr dt]
  {user usr data dt})
