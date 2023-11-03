(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
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

(defn- eval-pattern-as-dataflow [evaluator pat]
  (let [evt-name (li/make-path repl-component (li/unq-name))]
    (ln/event evt-name {})
    (try
      (let [evobj {evt-name {}}]
        (ln/dataflow evt-name pat)
        (evaluator
         (if-let [env (get-active-env)]
           (cn/assoc-event-context-env env evobj)
           evobj)))
      (finally
        (cn/remove-event evt-name)))))

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

(defn- need-schema-init? [exp]
  (let [n (first exp)]
    (or (= n 'entity)
        (and (= n 'relationship)
             (let [rec-def (if (map? (second exp))
                             (li/record-attributes (second exp))
                             (nth exp 2))]
               (:between (:meta rec-def)))))))

(defn- maybe-reinit-schema [store exp]
  (when (and (seqable? exp) (need-schema-init? exp))
    (let [[c _] (li/split-path
                 (let [r (second exp)]
                   (if (map? r)
                     (li/record-name r)
                     r)))]
      (store/force-init-schema store c)))
  exp)

(defn- repl-eval [store evaluator exp]
  (try
    (let [r (eval exp)]
      (when (maybe-reinit-schema store exp)
        (if (or (map? r) (vector? r) (keyword? r))
          (evaluate-pattern evaluator r)
          r)))
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex))))))

(defn- infer-model-name []
  (try
    (:name (loader/load-default-model-info))
    (catch Exception ex
      (println (str "WARN - " (.getMessage ex)))
      :fractl)))

(defn run [model-name store evaluator]
  (let [model-name (or model-name (infer-model-name))
        prompt (str (name model-name) "> ")
        reval (partial repl-eval store evaluator)]
    (use '[fractl.lang])
    (ln/component repl-component)
    (loop []
      (print prompt) (flush)
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
                (recur))

            :else
            (do
              (when-let [r (reval exp)]
                (pp/pprint r)
                (flush))
              (recur)))
          (recur))))))
