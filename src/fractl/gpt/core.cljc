(ns fractl.gpt.core
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [fractl.datafmt.json :as json]
            [fractl.global-state :as gs]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.util :as u]
            [fractl.util.http :as http]
            [fractl.util.logger :as log])
  #?(:clj (:require [clojure.java.io :as io])))

#?(:clj
   (defn read-and-parse-edn-file [filename]
     (with-open [reader (io/reader filename)]
       (edn/read-string (slurp reader)))))

#?(:clj
   (def ^:private resolver-conversation (read-and-parse-edn-file "fractl/extn/llm/resolver_seed.edn"))
   :cljs
   (def ^:private resolver-conversation nil))

(defn add-to-conversation
  ([history role s]
   (concat history [{:role role :content s}]))
  ([s]
   (add-to-conversation resolver-conversation "user" s)))

(defn post
  ([gpt result-handler request-message request-tunings]
   (http/do-post
     "https://api.openai.com/v1/chat/completions"
     {:headers {"Content-Type"  "application/json"
                "Authorization" (str "Bearer " (:api-key gpt))}}
     (into {:model    (:gpt-model gpt)
            :messages request-message}
           (when-not (nil? request-tunings)
             request-tunings))
     :json (fn [{body :body :as response}]
             (let [decoded-body (json/decode body)
                   choices (:choices decoded-body)]
               (if (not (nil? decoded-body))
                 {:chat-response (get-in (first choices) [:message :content])}
                 (u/throw-ex "AI failed to service your request, please try again"))))))
  ([gpt result-handler request-message]
   (post gpt result-handler request-message nil)))

(defn- choices [resp]
  (map #(:content (:message %)) (:choices resp)))

(def default-model "gpt-3.5-turbo")

(defn init-gpt [gpt-model-name api-key]
  {:gpt-model gpt-model-name
   :api-key api-key})

(defn- interactive-generate-helper [type gpt response-handler request]
  (let [request (if (and (string? request) (= type "resolver"))
                  (add-to-conversation request)
                  request)]
    (post gpt (fn [r]
                (when-let [[choice next-request] (response-handler
                                                   (choices (:chat-response r)))]
                  (interactive-generate-helper
                    type gpt response-handler (add-to-conversation
                                           (add-to-conversation request "assistant" choice)
                                           "user" next-request))))
          request)))

(defn interactive-generate
  ([type gpt-model-name api-key response-handler request]
   (interactive-generate-helper type (init-gpt gpt-model-name api-key) response-handler request))
  ([type response-handler request]
   (interactive-generate type default-model (u/getenv "OPENAI_API_KEY") response-handler request)))

(declare maybe-intern-component)

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

(defn non-interactive-generate-helper
  ([type gpt response-handler request-message request-tunings]
   (let [request (if (and (string? request-message) (= type "resolver"))
                   (add-to-conversation request-message)
                   request-message)]
     (post gpt (fn [r]
                 (let [choices (choices (:chat-response r))
                       [choice err-msg] (find-choice choices)]
                   (if-not err-msg
                     (response-handler choice (add-to-conversation request "assistant" choice))
                     (do (log/warn (str "attempt to intern component failed: " err-msg))
                         (response-handler nil nil)))))
           request
           request-tunings)))
  ([type gpt response-handler request-message]
   (non-interactive-generate-helper type gpt response-handler request-message nil)))

(defn non-interactive-generate
  ([type gpt-model-name api-key response-handler request-message request-tunings]
   (non-interactive-generate-helper type (init-gpt gpt-model-name api-key) response-handler request-message request-tunings))
  ([type gpt-model-name api-key response-handler request-message]
   (non-interactive-generate-helper type (init-gpt gpt-model-name api-key) response-handler request-message))
  ([type api-key response-handler request-message]
   (if (nil? api-key)
     (non-interactive-generate type default-model (u/getenv "OPENAI_API_KEY") response-handler request-message)
     (non-interactive-generate type default-model api-key response-handler request-message)))
  ([type response-handler request-message]
   (non-interactive-generate type default-model (u/getenv "OPENAI_API_KEY") response-handler request-message)))

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
  #?(:clj
     (let [final-s (read-string (str "(do " (remove-invalid-tokens (trim-to-exp s)) ")"))]
       (when-let [exps (seq (filter model-exp? final-s))]
         (doseq [exp exps]
           (if (clj-exp? exp)
             (li/evaluate exp)
             (apply (get model-fns (first exp)) (rest exp))))
         `(do ~@exps)))))

(defn- print-choice [s input-cache]
  (try
    (if-let [c (maybe-intern-component s)]
      (#?(:clj clojure.pprint/pprint :cljs println) c)
      (prnf s))
    (catch #?(:clj Exception :cljs :default) ex
      (let [msg #?(:clj (str (.getMessage ex)
                             (ex-data ex))
                   :cljs ex)
            s (str "ERROR: " msg)]
        (when-not (set-cache input-cache s)
          (reset-input-cache! input-cache)
          (prnf (str s ", choice: " s)))))))

;; API for cljs clients

(defn add-user-message [history msg]
  (add-to-conversation history "user" msg))

(def choice :choice)
(def chat-response :chat-response)
(def chat-history :chat-history)

(defn- make-model [component-def]
  (let [cname (second component-def)
        c (first (s/split (str cname) #"\."))]
    {:name (keyword (second (s/split c #"\:")))
     :version "0.0.1"
     :fractl-version (or (gs/fractl-version) "current")
     :components [cname]}))

(defn- verify-component-defs [exps]
  (doseq [exp exps]
    (when-let [f (get model-fns (first exp))]
      (apply f (rest exp))))
  exps)

(defn get-model [chat-response]
  (try
    (let [exps (rest (u/parse-string (str "(do " chat-response ")")))]
      (when (= 'component (ffirst exps))
        {:model     (make-model (first exps))
         :component (verify-component-defs exps)}))
    (catch #?(:clj Exception :cljs :default) ex
      (log/warn ex))))

#?(:clj
   (defn bot [request]
     (let [type (:type request)
           req [{:role "user" :content (:content request)}]
           resp (atom nil)]
       (non-interactive-generate
         type
        (fn [choice history]
          (reset!
           resp
           {:choice choice
            :chat-history history}))
        req)
       (get-model @resp))))
