(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as s]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.loader :as loader]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.store :as store]
            [fractl.util :as u]   
            [fractl.util.logger :as log]
            [fractl.env :as env]
            [fractl.global-state :as gs]))

(def ^:private repl-component :Fractl.Kernel.Repl)
(def ^:private ^ThreadLocal active-env (ThreadLocal.))

(defn- set-active-env! [env]
  (.set active-env env))

(defn- get-active-env []
  (.get active-env))

(defn- invoke-eval [evaluator evobj]
  (evaluator
   (if-let [env (get-active-env)]
     (cn/assoc-event-context-env env evobj)
     evobj)))

(defn- event-instance? [obj]
  (and (map? obj)
       (cn/event? (li/record-name obj))))

(defn- eval-pattern-as-dataflow [evaluator pat]
  (if (event-instance? pat)
    (invoke-eval evaluator pat)
    (let [evt-name (li/make-path repl-component (li/unq-name))]
      (ln/event evt-name {})
      (try
        (let [evobj {evt-name {}}]
          (ln/dataflow evt-name pat)
          (invoke-eval evaluator evobj))
        (finally
          (cn/remove-event evt-name))))))

(defn- evaluate-pattern [evaluator pat]
  (if (keyword? pat)
    (or (env/lookup (get-active-env) pat) pat)
    (try
      (let [res (eval-pattern-as-dataflow evaluator pat)
            {env :env status :status result :result}
            (if (map? res) res (first res))]
        (if (= :ok status)
          (do (ev/fire-post-events env)
              (set-active-env! (env/disable-post-event-triggers env))
              result)
          (or status pat)))
      (catch Exception ex
        (println (str "WARN - " (.getMessage ex)))
        pat))))

(defn- proc-event-pattern [pat]
  (when (:as pat)
    (u/throw-ex "alias not supported for event-pattern"))
  pat)

(defn- reldef [exp]
  (if (map? (second exp))
    (li/record-attributes (second exp))
    (nth exp 2)))

(defn- need-schema-init? [exp]
  (let [n (first exp)]
    (or (= n 'entity)
        (= n 'relationship))))

(defn- maybe-reinit-schema [store exp]
  (when (and (seqable? exp) (need-schema-init? exp))
    (let [n (let [r (second exp)]
              (if (map? r)
                (li/record-name r)
                r))
          [c _] (li/split-path n)
          is-rel (= (first exp) 'relationship)
          is-ent (not is-rel)
          rd (when is-rel (reldef exp))]
      (when (or is-ent (:between (:meta rd)))
        (store/drop-entity store n))
      (when is-rel
        (when-let [spec (:contains (:meta rd))]
          (store/drop-entity store (second spec))))
      (store/force-init-schema store c)))
  exp)

(def ^:private lang-fns #{'entity 'relationship 'event 'record 'dataflow})

(defn- lang-defn? [exp]
  (and (seqable? exp)
       (some #{(first exp)} lang-fns)))

(defn- repl-eval [store evaluator exp]
  (try
    (let [r (eval exp)]
      (when (maybe-reinit-schema store exp)
        (cond
          (lang-defn? exp) r
          (or (map? r) (vector? r) (keyword? r))
          (evaluate-pattern evaluator r)
          :else r)))
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex))))))

(defn- infer-model-name []
  (try
    (:name (loader/load-default-model-info))
    (catch Exception ex
      (println (str "WARN - " (.getMessage ex)))
      :fractl)))

(defn- maybe-change-model-name [model-name exp]
  (if (and (seqable? exp) (= 'component (first exp)))
    (let [n (second exp)
          s (s/split (subs (str n) 1) #"\.")]
      (s/lower-case (first s)))
    model-name))

(defn run [model-name store evaluator]
  (let [model-name (or model-name (infer-model-name))
        reval (partial repl-eval store evaluator)]
    (use '[fractl.lang])
    (use '[fractl.lang.tools.replcmds])
    (ln/component repl-component)
    (loop [model-name model-name]
      (print (str (name model-name) "> ")) (flush)
      (let [exp (try
                  (edn/read (java.io.PushbackReader.
                             (java.io.BufferedReader. *in*)))
                  (catch Exception ex
                    (println (str "ERROR in input - " (.getMessage ex)))))]
        (if exp
          (cond
            (= exp :quit)
            (do
              (println 'bye)
              (System/exit 0))

            (= exp '?)
            (do (pp/pprint
                 {:fractl-version (gs/fractl-version)
                  :components (vec (cn/component-names))})
                (recur model-name))

            :else
            (do
              (when-let [r (reval exp)]
                (pp/pprint r)
                (flush))
              (recur (maybe-change-model-name model-name exp))))
          (recur model-name))))))
