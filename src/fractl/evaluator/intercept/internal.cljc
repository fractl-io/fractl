(ns fractl.evaluator.intercept.internal)

(def iname :name)
(def ifn :fn)

(defn make-interceptor [name fn]
  {iname :rbac ifn fn})

(def event :event)
(def data :data)

(defn encode-arg [evt dt]
  {event evt data dt})
