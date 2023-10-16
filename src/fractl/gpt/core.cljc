(ns fractl.gpt.core
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.util.http :as http]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.global-state :as gs]
            [fractl.datafmt.json :as json]
            [fractl.gpt.seed :as seed]))

(def ^:private default-conversation seed/simple-conversation)

(defn add-to-conversation
  ([history role s]
   (concat history [{:role role :content s}]))
  ([msgs]
   (concat default-conversation msgs)))

(defn post [gpt result-handler request]
  (http/do-post
   "https://api.openai.com/v1/chat/completions"
   {:headers {"Content-Type" "application/json"
              "Authorization" (str "Bearer " (:api-key gpt))}}
   {:model (:gpt-model gpt)
    :messages request
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

(def default-model "gpt-3.5-turbo")

(defn init-gpt [gpt-model-name api-key]
  {:gpt-model gpt-model-name
   :api-key api-key})

(defn- interactive-generate-helper [gpt response-handler request]
  (let [request (add-to-conversation request)]
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

(declare maybe-intern-component)

(def ^:private MAX-RETRIES 3)

(defn- find-choice [choices]
  (try
    (loop [choices choices]
      (if-let [c (first choices)]
        [c nil]
        #_(if (maybe-intern-component c)
          [c nil]
          (recur (rest choices)))
        [nil "no valid choices found in response"]))
    (catch #?(:clj Exception :cljs :default) ex
      [nil #?(:clj (.getMessage ex) :cljs ex)])))

(defn non-interactive-generate-helper [gpt response-handler request]
  (let [orig-request request
        request (add-to-conversation request)]
    (post gpt (fn [r]
                (let [choices (choices (:chat-response r))
                      [choice err-msg] (find-choice choices)]
                  (if-not err-msg
                    (response-handler choice (add-to-conversation orig-request "assistant" choice))
                    (do (log/warn (str "attempt to intern component failed: " err-msg))
                        (response-handler nil nil)))))
          request)))

(defn non-interactive-generate
  ([gpt-model-name api-key response-handler request]
   (non-interactive-generate-helper (init-gpt gpt-model-name api-key) response-handler request))
  ([response-handler request]
   (non-interactive-generate default-model (u/getenv "OPENAI_API_KEY") response-handler request)))

(defn- prnf [s]
  #?(:clj
     (do (print s) (flush))
     :cljs (println s)))

(defn- make-input-cache [threshold]
  (atom {:threshold threshold :count 0 :cache nil}))

(defn- set-cache [input-cache s]
  (let [c @input-cache, n (:count c)]
    (when (<= n (:threshold c))
      (swap! input-cache assoc :count (inc n) :cache s)
      input-cache)))

(defn- fetch-cache [input-cache]
  (when-let [s (:cache @input-cache)]
    (swap! input-cache assoc :cache nil)
    s))

(defn- reset-input-cache! [input-cache]
  (swap! input-cache assoc :count 0 :cache nil))

(defn- prompt-for-input
  ([prompt input-cache]
   #?(:clj
      (or (and input-cache (fetch-cache input-cache))
          (do (prnf prompt) (read-line)))))
  ([prompt]
   (prompt-for-input prompt nil)))

(def ^:private model-fns
  {'component ln/component
   'entity ln/entity
   'record ln/record
   'event ln/event
   'relationship ln/relationship
   'dataflow ln/dataflow
   'attribute ln/attribute})

(defn- exp? [tags x]
  (and (seqable? x)
       (some #{(first x)} tags)))

(def ^:private clj-constructs #{'defn- 'defn 'def})

(def ^:private clj-exp? (partial exp? clj-constructs))

(def ^:private model-constructs (concat (keys model-fns) clj-constructs))

(def ^:private model-exp? (partial exp? model-constructs))

(defn- remove-invalid-tokens [s]
  (s/join " " (filter #(not (s/ends-with? % ":")) (s/split s #" "))))

(defn- trim-to-exp [s]
  (if-let [i (s/index-of s "(")]
    (subs s i)
    s))

(defn maybe-intern-component [s]
  (log/debug (str "trying to intern choice: " s))
  (let [final-s (read-string (str "(do " (remove-invalid-tokens (trim-to-exp s)) ")"))]
    (when-let [exps (seq (filter model-exp? final-s))]
      (doseq [exp exps]
        (if (clj-exp? exp)
          (li/evaluate exp)
          (apply (get model-fns (first exp)) (rest exp))))
      `(do ~@exps))))

(defn- print-choice [s input-cache]
  (try
    (if-let [c (maybe-intern-component s)]
      (clojure.pprint/pprint c)
      (prnf s))
    (catch #?(:clj Exception :cljs :default) ex
      (let [msg #?(:clj (str (.getMessage ex)
                             (ex-data ex))
                   :cljs ex)
            s (str "ERROR: " msg)]
        (when-not (set-cache input-cache s)
          (reset-input-cache! input-cache)
          (prnf (str s ", choice: " s)))))))

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
      (let [error-cache (make-input-cache 3)]
        (bot (partial prompt-for-input "? " error-cache)
             #(do (print-choice % error-cache) true)
             app-description))
      :cljs (u/throw-ex (str "no default bot implementation"))))
  ([] (bot (prompt-for-input "Enter app-description: "))))

;; API for cljs clients

(defn add-user-message [history msg]
  (add-to-conversation history "user" msg))

(def choice :choice)
(def chat-history :chat-history)

(defn- make-model [component-def]
  (let [cname (second component-def)
        c (first (s/split (str cname) #"\."))]
    {:name (keyword (subs (s/lower-case c) 1))
     :version "0.0.1"
     :fractl-version (or (gs/fractl-version) "current")
     :components [cname]}))

(defn- verify-component-defs [exps]
  (doseq [exp exps]
    (when-let [f (get model-fns (first exp))]
      (apply f (rest exp))))
  exps)

(defn get-model [response]
  (when-let [c (choice response)]
    (let [exps (rest (u/parse-string (str "(do " c ")")))]
      (when (= 'component (ffirst exps))
        {:model (make-model (first exps))
         :component (verify-component-defs exps)}))))
