(ns fractl.inference
  (:require [clojure.edn :as edn]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.env :as env]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.inference.provider :as provider]
            [fractl.inference.provider.openai]
            #?(:clj [fractl.util.logger :as log]
               :cljs [fractl.util.jslogger :as log])
            [fractl.util.http :as uh]))

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

(def ^:dynamic mock-ai false)

(defn run-inference
  ([service-url {appid :app-id chatid :chat-id
                 use-docs :use-docs use-schema :use-schema
                 :or {use-docs true
                      use-schema true}}
    question context]
   (when-not question
     (u/throw-ex "inference cannot run without a question"))
   (let [r0 {:AppUuid (or appid (u/get-app-uuid))
             :ChatUuid (or chatid (u/uuid-string))
             :AgentConfig (:agent-config context)
             :QuestionOptions {:UseDocs use-docs
                               :UseSchema use-schema}
             :Question question}
         r (if context (assoc r0 :QuestionContext context) r0)
         is-review-mode (when (map? context) (get-in context [:EventContext :evaluate-inferred-patterns]))
         req {:Fractl.Inference.Service/Question r}
         out (if mock-ai
               [{:result [req]}]
               (binding [provider/current-provider (get-in context [:agent-config :config :provider])]
                 (ev/eval-all-dataflows
                  {:Fractl.Inference.Service/Create_Question
                   {:Instance req}})))
         result (-> out
                    first
                    :result
                    first)]
     (if mock-ai
       result
       (let [result (or (:QuestionResponse result) result)
             result (if (string? result) (edn/read-string result) result)]
         (if-let [patterns (:patterns result)]
           (if is-review-mode
             patterns
             (eval-patterns patterns (:inference-event context)))
           (u/throw-ex (:errormsg result)))))))
  ([question] (run-inference nil nil question nil))
  ([question context] (run-inference nil nil question context))  
  ([request question context] (run-inference nil request question context)))

(defn run-inference-for-event [question agent-config event-instance]
  (let [agent-config (cond
                       (map? agent-config) agent-config
                       (li/quoted? agent-config) (second agent-config))]
    (when (and agent-config (not (map? agent-config)))
      (u/throw-ex (str "invalid agent config: " agent-config)))
    (run-inference question {:inference-event event-instance
                             :agent-config agent-config})))
