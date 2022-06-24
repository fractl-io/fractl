(ns fractl.evaluator.intercept.core
  (:require [fractl.util :as u]
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
  (let [n (:name spec) f (:fn spec)]
    (if (and n f)
      (u/call-and-set
       interceptors
       (fn []
         (if-not (some #{n} (mapv :name @interceptors))
           spec
           (u/throw-ex (str "duplicate interceptor - " n)))))
      (u/throw-ex (str ":name and :fn required in interceptor spec - " spec)))))

(defn do-operation [opr user data]
  (loop [ins @interceptors
         result (ii/encode-arg user data)]
    (if-let [i (first ins)]
      (if-let [r ((:fn i) opr result)]
        (recur (rest ins) r)
        (u/throw-ex (str "operation " opr " blocked by interceptor " (:name i))))
      (ii/data result))))

(def read-operation (partial do-operation :read))
(def upsert-operation (partial do-operation :upsert))
(def delete-operation (partial do-operation :delete))
(def eval-operation (partial do-operation :eval))
