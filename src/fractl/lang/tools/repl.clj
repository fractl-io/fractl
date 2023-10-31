(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.loader :as loader]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.env :as env]
            [fractl.global-state :as gs]))

(def ^:private ^ThreadLocal active-env (ThreadLocal.))

(defn- evaluate-pattern [pat]
  (if (keyword? pat)
    (or (env/lookup (.get active-env) pat) pat)
    (try
      (let [res (ev/evaluate-pattern (.get active-env) pat)
            {env :env status :status result :result} res]
        (if (= :ok status)
          (do (.set active-env env)
              result)
          pat))
      (catch Exception ex pat))))

(defn- extract-result [res]
  (cond
    (map? res)
    (or (:result res) res)

    (vector? res)
    (extract-result (first res))

    :else res))

(defn- repl-eval [evaluator exp]
  (try
    (let [r (eval exp)]
      (if (or (map? r) (vector? r) (keyword? r))
        (if (map? r)
          (let [n (li/record-name r)]
            (if (cn/event? n)
              (extract-result (evaluator r))
              (evaluate-pattern r)))
          (evaluate-pattern r))
        r))
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex))))))

(defn- infer-model-name []
  (try
    (:name (loader/load-default-model-info))
    (catch Exception ex
      (println (str "WARN - " (.getMessage ex)))
      :fractl)))

(defn run [model-name evaluator]
  (let [model-name (or model-name (infer-model-name))
        prompt (str (name model-name) "> ")
        reval (partial repl-eval evaluator)]
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
