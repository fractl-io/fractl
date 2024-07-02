(ns fractl.inference.service.lib.prompt
  (:require [clj-time.core :as t]
            [clojure.string :as string]
            [cheshire.core :as json]
            [stringer.core :as stringer]
            [fractl.inference.util :as util]))

(defn make-system-message [system-message-text]
  {:role :system :content system-message-text})

(defn make-user-message [user-message-text]
  {:role :user :content user-message-text})

(defn make-assistant-message [assistant-message-text]
  {:role :assistant :content assistant-message-text})

(defn make-few-shot-messages [user-assistant-text-pairs]
  (reduce (fn [result [user-text assistant-text]]
            (conj result
                  (make-user-message user-text)
                  (make-assistant-message assistant-text)))
          []
          user-assistant-text-pairs))

(def classify-primary-secondary-template
  "
You will be provided with {question-type}. The {question-type} will be
delimited with {delimiter} characters.

Classify each query into a primary category and secondary category.
Provide your output in JSON format with the keys: primary and secondary.

Primary categories: [{primary-categories-comma-separated}]

{primary-secondary-categories}
")

(def classify-primary-secondary-delimiter "####")

(defn make-classify-primary-secondary-text
  "Make prompt text for classifying primary-secondary category of a question.
  Options:
    :question-type                String
    :primary-secondary-categories Map{primary [secondary]}
  "
  [{:keys [question-type primary-secondary-categories]}]
  (assert (string? question-type))
  (assert (map? primary-secondary-categories))
  (let [primary-categories (->> (keys primary-secondary-categories)
                                (string/join ", "))
        format-sub-categories (fn [primary-category secondary-categories]
                                (reduce (fn [text each-secondary-category]
                                          (str text each-secondary-category "\n"))
                                        (str primary-category " categories:\n")
                                        secondary-categories))
        formatted-categories (reduce-kv (fn [text primary-category secondary-categories]
                                          (str text
                                               (format-sub-categories primary-category
                                                                      secondary-categories)
                                               "\n"))
                                        ""
                                        primary-secondary-categories)]
    (stringer/nrender classify-primary-secondary-template
                      {:question-type question-type
                       :delimiter classify-primary-secondary-delimiter
                       :primary-categories-comma-separated primary-categories
                       :primary-secondary-categories formatted-categories})))

(defn make-classifier-primary-secondary-prompt
  "Return a function:

      (fn [user-question]) -> primary/secondary categories as JSON text

  Note: You must parse the response-text as JSON to obtain values
  See: make-classify-primary-secondary-text for arguments"
  [options]
  (let [system-message (make-system-message (make-classify-primary-secondary-text options))]
    (fn [user-question]
      [system-message
       (-> "{delimiter}{user-question}{delimiter}"
           (stringer/nrender {:delimiter classify-primary-secondary-delimiter
                              :user-question user-question})
           make-user-message)])))

(def classify-intent-delimiter "####")

(def classify-intent-system-template
  "You will be provided with user queries delimited with {delimiter} characters.

Classify each query into its constituents, such as intent and parameters
in a JSON format as follows:
```
{\"intent\": \"what the user wants\",
 \"params\": \"parameters, conditions, scope of the ask\"}
```
  ")

(def classify-intent-user-template
  "Background: {background}
Question: {delimiter}{question}{delimiter}")

(def args-schema-make-classify-intent-messages
  [:map
   [:few-shot-params {:default []} [:vector [:map
                                             [:user-background :map]
                                             [:user-question :string]
                                             [:assistant-intent :string]
                                             [:assistant-params :map]]]]
   [:background {:default {}} :map]
   [:question :string]])

(def args-validator-make-classify-intent-messages
  (util/make-validator-explainer args-schema-make-classify-intent-messages))


(defn make-classify-intent-messages [options]
  (let [{:keys [few-shot-params
                background
                question]} (args-validator-make-classify-intent-messages options)
        system-message (-> classify-intent-system-template
                           (stringer/nrender {:delimiter classify-intent-delimiter})
                           make-system-message)
        few-shot-messages (->> few-shot-params
                               (reduce (fn [user-assistant-text-pairs {:keys [user-background
                                                                              user-question
                                                                              assistant-intent
                                                                              assistant-params]
                                                                       :as   shot-params}]
                                         (let [user-text (stringer/nrender classify-intent-user-template
                                                                           {:background user-background
                                                                            :delimiter  classify-intent-delimiter
                                                                            :question   user-question})
                                               assistant-text (json/generate-string
                                                                {:intent assistant-intent
                                                                 :params assistant-params})]
                                           (conj user-assistant-text-pairs
                                                 [user-text
                                                  assistant-text])))
                                       [])
                               make-few-shot-messages)
        final-user-message (-> classify-intent-user-template
                               (stringer/nrender {:background (-> background
                                                                  (assoc :date-today (str (t/today)))
                                                                  json/generate-string)
                                                  :delimiter classify-intent-delimiter
                                                  :question question})
                               make-user-message)]
    (-> [system-message]
        (concat few-shot-messages)
        vec
        (conj final-user-message))))

(def docs-template
  "Given the following information:
```
{all-docs}
```")

(defn render-docs-template [all-docs]
  (->> all-docs
       (string/join "\n")
       (array-map :all-docs)
       (stringer/nrender docs-template)))

(def docs-rag-template
  "{docs-context}

Based on the above information, analyze and answer the following question:

Background (as JSON): {background}
Question: {question}
Answer:

Share the reasoning for your answer.")

(def args-validator-make-docs-rag-text
  (util/make-validator-explainer
    [:map
     [:all-docs [:vector :string]]
     [:background {:optional true} :map]
     [:user-question :string]]))

(defn make-docs-rag-text [options]
  (let [{:keys [all-docs
                background
                user-question]} (args-validator-make-docs-rag-text options)]
    (stringer/nrender docs-rag-template {:docs-context (render-docs-template all-docs)
                                         :background (-> background
                                                         (assoc :date-today (str (t/today)))
                                                         json/generate-string)
                                         :question user-question})))

(defn make-docs-rag-messages [options]
  [(make-system-message (make-docs-rag-text options))])

(def planner-template
  "Answer the following questions as best you can.
You have access to the following tools (which are not data) described in JSON format:

{all-tools}

{docs-context}

Use the following format:

Question: the input question you must answer

Thought: you should always think about what to do.

Action: the action to take, should be one of [{all-tool-names}]
Action Input: action input in JSON format (state references in reference.name notation)
Action Result: action result name, for example actionResult_1

Observation: the result of the action

... (this Thought/Action/Action Input/Action Result/Observation can repeat N times)
Thought: I now know the final answer
Final Answer: the final answer to the original input question

Begin!

Background (as JSON): {background}
Question: {question}
Thought: {scratchpad}")

(def schema-for-tool-spec
  (let [schema-for-params [:map
                           [:name string?]
                           [:type [:map
                                   [:type :string]
                                   [:format {:optional true} :string]]]
                           [:required boolean?]]]
    [:map
     [:description string?]
     [:params [:vector schema-for-params]]
     [:df-patterns vector?]]))

(def args-schema-make-planner-text
  [:map
   [:all-docs {:default []} [:vector :string]]
   [:all-tools [:map-of
                :string schema-for-tool-spec]]])

(def args-validator-make-planner-text
  (util/make-validator-explainer args-schema-make-planner-text))

(defn make-planner-text [options]
  (let [{:keys [all-docs
                all-tools]} (args-validator-make-planner-text options)
        tool-names (keys all-tools)
        comma-separated-tool-names (string/join ", " tool-names)
        tool-template "{tool-name}: {tool-details}"
        tool-details (->> all-tools
                          (reduce-kv (fn [rows tool-name tool-spec]
                                       (->> {:tool-name    tool-name
                                             :tool-details (-> tool-spec
                                                               (dissoc :df-patterns)
                                                               json/generate-string)}
                                            (stringer/nrender tool-template)
                                            (conj rows)))
                                     [])
                          (string/join \newline))]
    (stringer/nrender planner-template {:docs-context (if (seq all-docs)
                                                        (render-docs-template all-docs)
                                                        "")
                                        :all-tools tool-details
                                        :all-tool-names comma-separated-tool-names})))

(defn make-planner-messages
  [options]
  (let [{:keys [background
                analysis-text
                user-question]} options]
    [(make-system-message (make-planner-text options))
     (-> "Background (as JSON): {background}
Analysis: {analysis}
Question: {question}
Thought:"
         (stringer/nrender {:background (-> background
                                            (assoc :date-today (str (t/today)))
                                            json/generate-string)
                            :analysis analysis-text
                            :question user-question})
         make-user-message)]))
