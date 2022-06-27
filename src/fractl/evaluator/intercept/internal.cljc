(ns fractl.evaluator.intercept.internal)

(def iname :name)
(def ifn :fn)

(defn make-interceptor [name fn]
  {iname :rbac ifn fn})

(def user :user)
(def data :data)

(defn encode-arg [usr dt]
  {user usr data dt})
