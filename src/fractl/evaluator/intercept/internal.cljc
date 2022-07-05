(ns fractl.evaluator.intercept.internal)

(def ^:private name-tag :name)
(def ^:private fn-tag :fn)

(defn make-interceptor [n fn]
  {name-tag n fn-tag fn})

(def intercept-name name-tag)
(def intercept-fn fn-tag)

(def event :event)
(def data-input :input)
(def data-output :output)

(defn encode-input-arg [evt data]
  {event evt
   data-input data})

(defn encode-output-arg [evt data]
  {event evt
   data-output data})

(defn- assoc-arg-value [k arg dt]
  (assoc arg k dt))

(def assoc-data-input (partial assoc-arg-value data-input))
(def assoc-data-output (partial assoc-arg-value data-output))
