(ns fractl.evaluator.intercept.internal)

(def ^:private name-tag :name)
(def ^:private fn-tag :fn)

(defn make-interceptor [n fn]
  {name-tag n fn-tag fn})

(def intercept-name name-tag)
(def intercept-fn fn-tag)

(def event :event)
(def continuation :continuation)
(def data-input :input)
(def data-output :output)

(defn encode-arg [evt input output cont]
  {event evt
   data-input input
   data-output output
   continuation cont})

(defn- assoc-arg-value [k arg dt]
  (assoc arg k dt))

(def assoc-continuation (partial assoc-arg-value continuation))
(def assoc-data-input (partial assoc-arg-value data-input))
(def assoc-data-output (partial assoc-arg-value data-output))
