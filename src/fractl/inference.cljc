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

(defn mock-mode? []
  (= "mock:ai" (System/getenv "INFERENCE_SERVICE_URL")))

(defn run-inference
  ([service-url {appid :app-id chatid :chat-id
                 use-docs :use-docs use-schema :use-schema
                 :or {use-docs true
                      use-schema true}}
    question context]
   (let [service-url (or service-url (System/getenv "INFERENCE_SERVICE_URL"))]
     (when-not service-url
       (u/throw-ex "AI inference url not configured"))
     (when-not question
       (u/throw-ex "inference cannot run without a question"))
     (let [r0 {:AppUuid (or appid (u/get-app-uuid))
               :ChatUuid (or chatid (u/uuid-string))
               :QuestionOptions {:UseDocs use-docs
                                 :UseSchema use-schema}
               :Question question}
           r (if context (assoc r0 :QuestionContext context) r0)
           is-review-mode (when (map? context) (get-in context [:EventContext :evaluate-inferred-patterns]))
           req {:Inference.Service/Question r}
           mock-ai (= service-url "mock:ai")
           out (if mock-ai
                 [{:result [req]}]
                 (uh/POST (str service-url "/api/Inference.Service/Question") nil req))
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
             (u/throw-ex (:errormsg result))))))))
  ([question] (run-inference nil nil question nil))
  ([question context] (run-inference nil nil question context))  
  ([request question context] (run-inference nil request question context)))

(defn run-inference-for-event [question event-instance]
  (run-inference question {:inference-event event-instance}))
