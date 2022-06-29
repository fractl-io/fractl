(ns fractl.evaluator.intercept.core
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.env :as env]
            [fractl.evaluator.intercept.internal :as ii]))

;; Manages a pipeline of interceptors.
;; Interceptors are executed in the order they are added
;; to the pipeline, the output of the first becoming the
;; input for the second and so on. The result of the final
;; interceptor will be returned to the caller - usually this
;; will be the root evaluator.
;;
;; An interceptor is represented by a map with
;; two keys - `:name` and `:fn`. `:name` uniquely
;; identifies the interceptor and `:fn` is a two-argument
;; function that performs the intercept operation. The first
;; argument identifies the operation, which should be
;; one of - `[:read :upsert :delete :eval]`. The second argument
;; must be a map with at least two keys - `:user` and `:data`.
;; `:user` is the name of the currently logged-in `:Kernel.RBAC/User`
;; and data could be an entity instance, a list of entity instances
;; or the name of an entity.
;;
;; An interceptor may or may not transform the data, but its
;; return value must be encoded in the same format as its second
;; argument, so that it can become the input for the next interceptor
;; in the pipeline. An interceptor may terminate the pipeline by
;; returning nil
(def ^:private interceptors (u/make-cell []))

(defn add-interceptor! [spec]
  (let [n (ii/intercept-name spec) f (ii/intercept-fn spec)]
    (if (and n f)
      (do (u/call-and-set
           interceptors
           (fn []
             (let [ins @interceptors]
               (if-not (some #{n} (mapv ii/intercept-name ins))
                 (conj ins spec)
                 (u/throw-ex (str "duplicate interceptor - " n))))))
          (ii/intercept-name spec))
      (u/throw-ex (str ii/intercept-name " and " ii/intercept-fn " required in interceptor spec - " spec)))))

(defn reset-interceptors! []
  (u/safe-set interceptors []))

(defn do-operation [opr event-instance direction data]
  (if-let [ins (seq @interceptors)]
    (loop [ins ins
           result (ii/encode-arg event-instance direction data)]
      (if-let [i (first ins)]
        (if-let [r ((ii/intercept-fn i) opr result)]
          (recur (rest ins) r)
          (u/throw-ex (str "operation " opr " blocked by interceptor " (ii/intercept-name i))))
        (ii/data result)))
    data))

(def ^:private read-operation (partial do-operation :read))
(def ^:private upsert-operation (partial do-operation :upsert))
(def ^:private delete-operation (partial do-operation :delete))
(def ^:private eval-operation (partial do-operation :eval))

(defn- do-intercept-opr [intercept-fn direction env data]
  (if (ii/valid-direction? direction)
    (if (env/interceptors-blocked? env)
      data
      (intercept-fn (env/active-event env) direction data))
    (u/throw-ex (str "invalid direction - " direction))))

(def read-intercept (partial do-intercept-opr read-operation ii/input))
(def upsert-intercept (partial do-intercept-opr upsert-operation ii/input))
(def delete-intercept (partial do-intercept-opr delete-operation ii/input))
(def eval-intercept (partial do-intercept-opr eval-operation ii/input))

(def read-result-intercept (partial do-intercept-opr read-operation ii/output))
(def upsert-result-intercept (partial do-intercept-opr upsert-operation ii/output))
(def delete-result-intercept (partial do-intercept-opr delete-operation ii/output))
(def eval-result-intercept (partial do-intercept-opr eval-operation ii/output))
