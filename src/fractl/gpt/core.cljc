(ns fractl.gpt.core
  (:require [clojure.string :as s]
            [fractl.util.http :as http]
            [fractl.util :as u]
            [fractl.lang :as ln]
            [fractl.datafmt.json :as json]
            [fractl.gpt.seed :as seed]))

(def ^:private default-conversation seed/conversation)

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
     (do (print s) (flush))
     :cljs (println s)))

(defn- prompt-for-input [prompt]
  #?(:clj
     (do (prnf prompt)
         (read-line))))

(def ^:private model-fns
  {'component ln/component
   'entity ln/entity
   'record ln/record
   'event ln/event
   'relationship ln/relationship
   'dataflow ln/dataflow
   'attribute ln/attribute})

(def ^:private model-constructs (keys model-fns))

(defn- model-exp? [x]
  (and (seqable? x)
       (some #{(first x)} model-constructs)))

(defn- remove-invalid-tokens [s]
  (s/join " " (filter #(not (s/ends-with? % ":")) (s/split s #" "))))

(defn- trim-to-exp [s]
  (if-let [i (s/index-of s "(")]
    (subs s i)
    s))

(defn maybe-intern-component [s]
  (let [final-s (read-string (str "(do " (remove-invalid-tokens (trim-to-exp s)) ")"))]
    (when-let [exps (seq (filter model-exp? final-s))]
      (doseq [exp exps]
        (apply (get model-fns (first exp)) (rest exp)))
      `(do ~@exps))))

(defn- print-choice [s]
  (try
    (if-let [c (maybe-intern-component s)]
      (clojure.pprint/pprint c)
      (prnf s))
    (catch #?(:clj Exception :cljs :default) ex
      (.printStackTrace ex)
      (let [msg #?(:clj (str (.getMessage ex)
                             (ex-data ex))
                   :cljs ex)]
        (prnf (str "ERROR: " msg ", choice: " s))))))

(defn bot
  ([prompt-for-input handle-choice app-description]
   (interactive-generate
    (fn [choices]
      (when-let [c (first choices)]
        (when (handle-choice c)
          (let [req (prompt-for-input)]
            (when-not (= req "bye")
              [c req])))))
    app-description))
  ([app-description]
   #?(:clj
      (bot (partial prompt-for-input "? ")
           #(do (print-choice %) true)
           app-description)
      :cljs (u/throw-ex (str "no default bot implementation"))))
  ([] (bot (prompt-for-input "Enter app-description: "))))
