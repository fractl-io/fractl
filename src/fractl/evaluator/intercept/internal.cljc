(ns fractl.evaluator.intercept.internal)

(def ^:private name-tag :name)
(def ^:private fn-tag :fn)

(defn make-interceptor [n fn]
  {name-tag n fn-tag fn})

(def intercept-name name-tag)
(def intercept-fn fn-tag)

(def event :event)
(def data :data)
(def direction :direction)

(defn encode-arg [evt dir dt]
  {event evt
   data dt
   direction dir})

(def input :input)
(def output :output)

(defn valid-direction? [d]
  (or (= d input) (= d output)))

(defn data-input? [obj]
  (= input (direction obj)))

(defn data-output? [obj]
  (= output (direction obj)))
