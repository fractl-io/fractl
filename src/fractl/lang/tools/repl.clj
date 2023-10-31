(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.loader :as loader]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.evaluator.root :as evr]
            [fractl.util :as u]
            [fractl.env :as env]
            [fractl.global-state :as gs]))

(defn- evaluate-pattern [pat]
  (if (keyword? pat)
    (or (env/lookup (gs/get-active-env) pat) pat)
    (try
      (let [res (ev/evaluate-pattern (gs/get-active-env) pat)
            {env :env status :status result :result} res]
        (if (= :ok status)
          (do (gs/set-active-env! env)
              result)
          (or status pat)))
      (catch Exception ex
        (println (str "WARN - " (.getMessage ex)))
        pat))))

(defn- proc-event-pattern [pat]
  (when (:as pat)
    (u/throw-ex "alias not supported for event-pattern"))
  pat)

(def ^:private lang-fns #{'entity 'relationship 'event 'record 'component})

(defn- maybe-reinit-schema [exp]
  (when (and (seqable? exp) (some #{(first exp)} lang-fns))
    (let [[c _] (li/split-path
                 (let [r (second exp)]
                   (if (map? r)
                     (li/record-name r)
                     r)))]
      (evr/reinit-component-schema! c)))
  exp)

(defn- repl-eval [exp]
  (try
    (let [r (eval (maybe-reinit-schema exp))]
      (if (or (map? r) (vector? r) (keyword? r))
        (evaluate-pattern r)
        r))
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex))))))

(defn- infer-model-name []
  (try
    (:name (loader/load-default-model-info))
    (catch Exception ex
      (println (str "WARN - " (.getMessage ex)))
      :fractl)))

(defn run [model-name]
  (let [model-name (or model-name (infer-model-name))
        prompt (str (name model-name) "> ")
        reval repl-eval]
    (use '[fractl.lang])
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
