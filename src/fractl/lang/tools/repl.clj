(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]))

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

(defn run [model-name evaluator]
  (let [prompt (str (name model-name) "> ")
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
          (if (= exp :quit)
            (do
              (println 'bye)
              (System/exit 0))
            (do
              (when-let [r (reval exp)]
                (pp/pprint r)
                (flush))
              (recur)))
          (recur))))))
