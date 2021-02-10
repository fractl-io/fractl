(ns fractl.resolver.core
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]))

(def ^:private valid-resolver-keys #{:upsert :delete :get :query :eval})

(defn make-resolver
  ([resolver-name fnmap eval-dataflow]
  (when-not (every? identity (map #(some #{%} valid-resolver-keys) (keys fnmap)))
    (u/throw-ex (str "invalid resolver keys - " (keys fnmap))))
  (doseq [[k v] fnmap]
    (when-not (fn? (:handler v))
      (u/throw-ex (str "resolver key " k " must be mapped to a function"))))
  (assoc fnmap
         :name resolver-name
         :evt-handler eval-dataflow))
  ([resolver-name fnmap]
   (make-resolver resolver-name fnmap nil)))

(def resolver-name :name)
(def resolver-upsert :upsert)
(def resolver-delete :delete)
(def resolver-query :query)
(def resolver-eval :eval)

(defn- ok? [r] (= :ok (:status r)))

(defn- ok-ffresult [r]
  (when (ok? r)
    (ffirst (:result r))))

(defn- apply-xform
  "Tranformations can be applied as the data moves from the evaluator to
   resolver, and vice versa. Transformations are specified as a vector in
   a `:xform` map in the resolver specification. `:in` represents the
   xforms of data flowing from evaluator to resolver and `:out` represents
   those in the reverse direction.

   xforms can be represented either by a clojure function or a fractl name.
   With a clojure function, the function is applied to the data (or the result
   of a previous transformation). With a fractl name, a event corresponding
   to the specified name is triggered and evaluated. The event body has a single
   field called `:Instance`, which carries the data (entity instance)."
  [xform eval-dataflow env arg]
  (cond
    (fn? xform)
    (xform env arg)

    (li/name? xform)
    (when eval-dataflow
      (let [evt-inst (cn/make-instance
                      {xform {:Instance arg}})
            result (eval-dataflow evt-inst)]
        (ok-ffresult (first result))))

    :else
    arg))

(defn- apply-xforms
  [xforms eval-dataflow env arg]
  (loop [xforms xforms arg arg]
    (if-let [xf (first xforms)]
      (recur (rest xforms)
             (apply-xform xf eval-dataflow env arg))
      arg)))

(defn- invoke-method [method resolver f env arg]
  (if-let [in-xforms (get-in resolver [method :xform :in])]
    (let [eval-dataflow (:evt-handler resolver)
          final-arg (apply-xforms in-xforms eval-dataflow env arg)
          result (f final-arg)]
      (if-let [out-xforms (get-in resolver [method :xform :out])]
        (apply-xforms out-xforms eval-dataflow env result)
        result))
    (let [eval-dataflow (:evt-handler resolver)
          result (f arg)]
      (if-let [out-xforms (get-in resolver [method :xform :out])]
        (apply-xforms out-xforms eval-dataflow env result)
        result))))

(defn- wrap-result [method resolver env arg]
  (when-let [m (get-in resolver [method :handler])]
    {:resolver (:name resolver)
     :method method
     :result (invoke-method method resolver m env arg)}))

(def call-resolver-upsert (partial wrap-result :upsert))
(def call-resolver-delete (partial wrap-result :delete))
(def call-resolver-query (partial wrap-result :query))
(def call-resolver-eval (partial wrap-result :eval))
