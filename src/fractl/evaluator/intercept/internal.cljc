(ns fractl.evaluator.intercept.internal)

(def ^:private name-tag :name)
(def ^:private fn-tag :fn)

(defn make-interceptor [n fn]
  {name-tag n fn-tag fn})

(def intercept-name name-tag)
(def intercept-fn fn-tag)

(def event :event)
(def data :data)
(def continuation :continuation)

(defn encode-arg [evt dt cont]
  {event evt
   data dt
   continuation cont})

(defn assoc-data [arg dt]
  (assoc arg data dt))
