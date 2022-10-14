(ns fractl.evaluator.intercept.internal
  (:require [fractl.lang.internal :as li]))

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

(defn attribute-ref? [path]
  (and (keyword? path)
       (seq (:refs (li/path-parts path)))))

(defn wrap-attribute [record-name attr-name]
  (let [p (if (keyword? record-name)
            record-name
            (li/make-path record-name))]
    (li/make-ref p attr-name)))

(def skip-for-input-tag :-*-skip-for-input-*-)
(def skip-for-output-tag :-*-skip-for-output-*-)
(def skip-for-output skip-for-output-tag)
(def skip-for-input skip-for-input-tag)
(def skip-for-input? (partial = skip-for-input))
(def skip-for-output? (partial = skip-for-output))
