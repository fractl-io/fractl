(ns fractl.inference
  (:require [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.http :as uh]))

(defn- register-event [component pats]
  (let [event-name (li/temp-event-name component)]
    (and (apply ln/dataflow event-name pats)
         event-name)))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn- eval-patterns [fractl-components fractl-patterns] 
  (let [event (-> (first fractl-components)
                  (register-event (as-vec fractl-patterns)))]
    (try
      (ev/safe-eval {event {}})
      (finally
        (cn/remove-event event)))))

(defn mock-mode? []
  (= "mock:ai" (System/getenv "COPILOT_URL")))

(defn run-inference
  ([service-url {appid :app-id chatid :chat-id
                 use-docs :use-docs use-schema :use-schema}
    question context]
   (let [service-url (or service-url (System/getenv "COPILOT_URL"))]
     (when-not service-url
       (u/throw-ex "AI inference url not configured"))
     (when-not question
       (u/throw-ex "inference cannot run without a question"))
     (let [r0 {:AppUuid (or appid (u/uuid-string))
               :ChatUuid (or chatid (u/uuid-string))
               :UseDocs use-docs
               :UseSchema use-schema
               :Question question}
           r (if context (assoc r0 :Context context) r0)
           req {:Copilot.Service.Core/PostAppQuestion r}
           mock-ai (= service-url "mock:ai")
           out (if mock-ai
                 [{:result [req]}]
                 (uh/POST (str service-url "/api/Copilot.Service.Core/PostAppQuestion") nil req))
           result (-> out
                      first
                      :result
                      first)]
       (if mock-ai
         result
         (if-let [patterns (:patterns result)]
           (let [components (remove #{:Fractl.Kernel.Identity :Fractl.Kernel.Lang
                                      :Fractl.Kernel.Store :Fractl.Kernel.Rbac
                                      :raw :-*-containers-*-}
                                    (cn/component-names))]
             (eval-patterns components patterns))
           (u/throw-ex (:errormsg result)))))))
  ([question] (run-inference nil nil question nil))
  ([question context] (run-inference nil nil question context))  
  ([request question context] (run-inference nil request question context)))
