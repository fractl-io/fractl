(ns fractl.gpt.core
  (:require [clojure.string :as s]
            [fractl.util.http :as http]
            [fractl.util :as u]
            [fractl.datafmt.json :as json]))

(def ^:private default-conversation (read-string (slurp "gpt/seed.edn")))

(defn- add-to-conversation
  ([history role s]
   (concat history [{:role role :content s}]))
  ([s]
   (add-to-conversation default-conversation "user" s)))

(defn- post [gpt result-handler request]
  (http/do-post
   "https://api.openai.com/v1/chat/completions"
   {:headers {"Content-Type" "application/json"
              "Authorization" (str "Bearer " (:api-key gpt))}}
   {:model (:gpt-model gpt)
    :messages request
    :max_tokens 1500
    :temperature 0.9
    :top_p 1
    :frequency_penalty 0.0
    :presence_penalty 0.6}
   :json (fn [{body :body :as response}]
           (result-handler
            (if (and (string? body) (seq body))
              {:chat-response (json/decode body)}
              response)))))

(defn- choices [resp]
  (map #(:content (:message %)) (:choices resp)))

(def ^:private default-model
  "gpt-3.5-turbo-16k"
  ;; "curie:ft-personal-2023-06-28-10-58-47"
  )

(defn init-gpt [gpt-model-name api-key]
  {:gpt-model gpt-model-name
   :api-key api-key})

;; Sample usage: (generate "a library application")
(defn generate
  ([gpt-model-name api-key request continuation]
   (let [gpt (init-gpt gpt-model-name api-key)]
     (post gpt continuation (add-to-conversation request))))
  ([app-request]
   (generate default-model (u/getenv "OPENAI_API_KEY") app-request println)))

(defn- interactive-generate-helper [gpt response-handler request]
  (let [request (if (string? request)
                  (add-to-conversation request)
                  request)]
    (post gpt (fn [r]
                (when-let [[choice next-request] (response-handler
                                                  (choices (:chat-response r)))]
                  (interactive-generate-helper
                   gpt response-handler (add-to-conversation
                                         (add-to-conversation request "assistant" choice)
                                         "user" next-request))))
          request)))

(defn interactive-generate
  ([gpt-model-name api-key response-handler request]
   (interactive-generate-helper (init-gpt gpt-model-name api-key) response-handler request))
  ([response-handler request]
   (interactive-generate default-model (u/getenv "OPENAI_API_KEY") response-handler request)))

(defn- prnf [s]
  #?(:clj
     (do (print s) (flush))))

(defn- prompt-for-input [prompt]
  #?(:clj
     (do (prnf prompt)
         (read-line))))

(defn bot []
  #?(:clj
     (interactive-generate
      (fn [choices]
        (when-let [c (first choices)]
          (println ">>>> " c)
          (let [req (prompt-for-input "? ")]
            (when-not (= req "bye")
              [c req]))))
      (prompt-for-input "Enter application description: "))))
