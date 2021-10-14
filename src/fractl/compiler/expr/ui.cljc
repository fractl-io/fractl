(ns fractl.compiler.expr.ui
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            #?(:cljs [reagent.core :as r])))

(defn- as-event-object [obj]
  (cond
    (map? obj)
    obj
    (li/name? obj)
    {obj {}}
    :else (u/throw-ex (str "invalid event - " obj))))

(declare process-spec)

(defn- eval-event [attr-names exp]
  `(let [x# ((fractl.evaluator/global-dataflow-eval)
             (fractl.component/make-instance
              ~(as-event-object
                (process-spec attr-names exp))))
         r# (first x#)]
     (if (ev/ok? r#)
       (first (:result r#))
       (u/throw-ex (str "dataflow failure " r#)))))

(defn- on-change-setter [setref]
  `(fn [obj#]
     (let [v# (-> obj# .-target .-value)
           inst# (deref (fractl.compiler.expr.ui/state-refer ~'instance))
           r# ~(if (seq (rest setref))
                 `(assoc-in inst# ~(vec setref) v#)
                 `(assoc inst# ~(first setref) v#))]
       (fractl.compiler.expr.ui/state-intern r#))))

(defn- make-setter-exp [attr-names exp]
  (cond
    (map? exp)
    (eval-event attr-names exp)

    (list? exp)
    (seq (mapv (partial process-spec attr-names) exp))

    :else (process-spec exp)))

(defn- value-setter [attr-names setref exp]
  (let [final-exp (make-setter-exp attr-names exp)]
    `(fn []
       (let [v# ~final-exp
             inst# (deref (fractl.compiler.expr.ui/state-refer ~'instance))
             ref# ~setref
             r# (if (seq (rest ref#))
                  (assoc-in inst# (vec ref#) v#)
                  (assoc inst# (first ref#) v#))]
         (fractl.compiler.expr.ui/state-intern r#)))))

(defn- process-set [attr-names elem]
  (let [parts (li/split-ref (second elem))]
    (when-not (some #{(first parts)} attr-names)
      (u/throw-ex (str (first parts) " not in context, cannot be :set")))
    (case (count elem)
      2 (on-change-setter parts)
      3 (value-setter attr-names parts (nth elem 2))
      (u/throw-ex (str "invalid :set " elem)))))

(defn- process-command [attr-names elem]
  (if (= :set (first elem))
    (process-set attr-names elem)
    (loop [xs elem, result []]
      (if-let [x (first xs)]
        (recur (rest xs) (conj result (process-spec attr-names x)))
        (vec result)))))

(defn- process-object [attr-names elem]
  (let [result (map (fn [[k v]]
                      [k (process-spec attr-names v)])
                    elem)]
    (into {} (doall result))))

(defn- process-spec [attr-names elem]
  (cond
    (li/name? elem)
    (let [parts (li/split-ref elem)]
      (if (some #{(first parts)} attr-names)
        (if (seq (rest parts))
          `(get-in (deref (fractl.compiler.expr.ui/state-refer ~'instance)) ~(vec parts))
          `(get (deref (fractl.compiler.expr.ui/state-refer ~'instance)) ~(first parts)))
        elem))

    (vector? elem)
    (process-command attr-names elem)

    (map? elem)
    (process-object attr-names elem)

    :else elem))

(defn compile-ui-spec [record-name attributes spec-attr-name spec]
  (let [code (process-spec (set (keys attributes)) (first (rest spec)))
        f `(fn [~'env ~'instance]
             (fn [~'instance]
               (fn [] ~code)))]
    (li/evaluate f)))

(def ^:private state-db (atom {}))

(defn state-refer
  ([instance update]
   (let [k (or (:Id instance) instance)
         old-cell (get @state-db k)
         cell (or old-cell (#?(:cljs r/atom :clj atom) instance))]
     (when (or update (not old-cell))
       (swap!
        state-db assoc k
        (if old-cell
          (do (reset! cell instance) cell)
          cell)))
     cell))
  ([instance] (state-refer instance false)))

(defn state-intern [instance]
  (state-refer instance true)
  instance)
