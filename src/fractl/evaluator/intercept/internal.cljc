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
(def interceptors :interceptors)

(defn encode-input-arg [evt data ins]
  {event evt
   data-input data
   interceptors (mapv name-tag ins)})

(defn encode-output-arg [evt data ins]
  {event evt
   data-output data
   interceptors (mapv name-tag ins)})

(defn- assoc-arg-value [k arg dt]
  (assoc arg k dt))

(def assoc-data-input (partial assoc-arg-value data-input))
(def assoc-data-output (partial assoc-arg-value data-output))

(defn has-instance-meta? [arg]
  (some #{:instance-meta} (interceptors arg)))

(defn apply-to-attribute [inst-type attr-name attr-value]
  {:attribute true
   :record inst-type
   :name attr-name
   :value attr-value})

(defn apply-to-attribute? [obj]
  (and (map? obj) (:attribute obj)))

(def attribute-record-type :record)
(def attribute-name :name)
(def attribute-value :value)
