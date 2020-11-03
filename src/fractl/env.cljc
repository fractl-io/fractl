(ns fractl.env
  "The environment of instance and variable bindings,
  used for pattern resolution."
  (:require [fractl.util :as u]
            [fractl.namespace :as n]))

(def EMPTY {})

;; !!NOTE!! This assertion may be removed once the
;; pattern-match algorithm is fully implemented.
;; This check ensures that the compiler never pushes
;; unparsed names of the form :Module/Entity to the runtime
;; environment.
(defn- assert-parsed-rec-name [rec-name]
  (if-not (vector? rec-name)
    (u/throw-ex (str "not a parsed record name - " rec-name))
    rec-name))

(defmacro getinsts
  "Fetch the instances of the specified record type.
   Ensures that only parsed record-names are passed to the
   runtime environment."
  [env rec-name]
  `(get ~env (assert-parsed-rec-name ~rec-name)))

(defn bind-instance [env rec-name instance]
  (let [insts (or (getinsts env rec-name) (list))]
    (assoc env rec-name (conj insts instance))))

(defn lookup-instance [env rec-name]
  (peek (getinsts env rec-name)))

(defn follow-reference [env path-parts]
  (let [recname [(:namespace path-parts) (:record path-parts)]
        inst (lookup-instance env recname)]
    (loop [env env, refs (:refs path-parts), obj inst]
      (if-let [r (first refs)]
        (let [x (get obj r)]
          (recur (if (n/an-instance? x)
                   (bind-instance env (n/parsed-instance-name x) x)
                   env)
                 (rest refs) x))
        [obj env]))))

(defn lookup-instances-by-attributes [env rec-name query-attrs]
  (when-let [insts (seq (getinsts env rec-name))]
    (filter #(every? (fn [[k v]] (= v (get % k))) query-attrs) insts)))

(def bind-variable assoc)
(def lookup-variable find)

(defn- objstack [env]
  (get env :objstack (list)))

(defn push-obj
  ([env rec-name obj]
   (let [stack (objstack env)]
     (assoc env :objstack (conj stack [rec-name obj]))))
  ([env rec-name] (push-obj env rec-name {})))

(defn peek-obj [env]
  (peek (objstack env)))

(defn pop-obj [env]
  (let [s (objstack env)]
    [(assoc env :objstack (pop s))
     (peek s)]))

(defn peek-obj-attribute [env n]
  (let [[_ obj] (peek-obj env)]
    (get obj n)))
