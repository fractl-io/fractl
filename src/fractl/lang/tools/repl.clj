(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [fractl.util.logger :as log]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.loader :as loader]
            [fractl.component :as cn]
            [fractl.global-state :as gs]))

(defn- repl-eval [evaluator exp]
  (try
    (let [r (eval exp)]
      (if (map? r)
        (let [n (li/record-name r)]
          (cond
            (cn/event? n) (evaluator r)
            (cn/find-object-schema n) (cn/make-instance r)
            :else r))
        r))
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex))))))

(defn- infer-model-name []
  (try
    (:name (loader/load-default-model-info))
    (catch Exception ex
      (log/warn (.getMessage ex))
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
                    (println (str "ERROR - " (.getMessage ex)))))]
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
