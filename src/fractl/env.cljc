(ns fractl.env
  "The environment of instance and variable bindings,
  used for pattern resolution."
  (:require [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.component :as cn]
            #?(:clj [net.cgrand.macrovich :as macros]))
  #?(:cljs (:require-macros [net.cgrand.macrovich :as macros]
                            [fractl.env :refer [getinsts]])))

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

(macros/deftime
  (defmacro getinsts
    "Fetch the instances of the specified record type.
   Ensures that only parsed record-names are passed to the
   runtime environment."
    [env rec-name]
    `(get ~env (assert-parsed-rec-name ~rec-name))))

(macros/usetime
 (defn bind-instance [env rec-name instance]
   (let [insts (or (getinsts env rec-name) (list))]
     (assoc env rec-name (conj insts instance)))))

(defn bind-instances [env rec-name instances]
  (su/move-all instances env #(bind-instance %1 rec-name %2)))

(macros/usetime
 (defn lookup-instance [env rec-name]
   (peek (getinsts env rec-name))))

(defn follow-reference [env path-parts]
  (let [recname [(:component path-parts) (:record path-parts)]
        inst (lookup-instance env recname)]
    (loop [env env, refs (:refs path-parts), obj inst]
      (if-let [r (first refs)]
        (let [x (get obj r)]
          (recur (if (cn/an-instance? x)
                   (bind-instance env (cn/parsed-instance-name x) x)
                   env)
                 (rest refs) x))
        [obj env]))))

(macros/usetime
 (defn lookup-instances-by-attributes [env rec-name query-attrs]
   (when-let [insts (seq (getinsts env rec-name))]
     (filter #(every? (fn [[k v]] (= v (get % k))) query-attrs) insts))))

(def bind-variable assoc)
(def lookup-variable find)

(defn- objstack [env]
  (get env :objstack (list)))

(defn push-obj
  "Push a single object or a sequence of objects to the stack"
  ([env rec-name x]
   (let [stack (objstack env)]
     (assoc env :objstack (conj stack [rec-name x]))))
  ([env rec-name] (push-obj env rec-name {})))

(defn peek-obj [env]
  (peek (objstack env)))

(defn pop-obj
  "Pop the object stack,
  return [updated-env single-object-flag? [name object]]"
  [env]
  (let [s (objstack env)
        [n obj :as x] (peek s)]
    [(assoc env :objstack (pop s))
     (map? obj) x]))
