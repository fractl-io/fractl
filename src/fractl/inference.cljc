(ns fractl.inference
  (:require [clojure.edn :as edn]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.env :as env]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.util :as u]
            #?(:clj [fractl.util.logger :as log]
               :cljs [fractl.util.jslogger :as log])
            [fractl.util.http :as uh]
            [fractl.inference.service.core :as inference]))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn- eval-patterns [fractl-patterns context-event]
  (let [env (env/bind-instance env/EMPTY context-event)]
    (loop [pats (as-vec fractl-patterns), env env, result nil]
      (if-let [p (first pats)]
        (let [r (ev/evaluate-pattern env p)]
          (if (u/safe-ok-result r)
            (recur (rest pats) (:env r) r)
            (do (log/error (str "inferred-pattern " p " failed with result " r))
                r)))
        (do (log/info (str "inference succeeded with result " result))
            result)))))

(defn run-inference-for-event [event agent-instance]
  (log/info (str "Processing response for inference " (cn/instance-type event)
                 " - " (u/pretty-str agent-instance)))
  (let [r0 (or (:Response agent-instance) agent-instance)
        result (if (string? r0) (edn/read-string r0) r0)
        is-review-mode (get-in event [:EventContext :evaluate-inferred-patterns])]
    (if-let [patterns (:patterns result)]
      (if is-review-mode
        patterns
        (eval-patterns patterns event))
      (u/throw-ex (:errormsg result)))))
