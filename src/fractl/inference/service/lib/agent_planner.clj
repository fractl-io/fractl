(ns fractl.inference.service.lib.agent-planner
  (:require [clojure.pprint :as pp]
            [clojure.string :as s]
            [clojure.walk :as walk]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.inference.service.lib.output-parser :as output-parser]))

(defn param-key?
  [k]
  (and (keyword? k)
       (= "param" (namespace k))))

(defn maybe-capitalize [^String s]
  (or (when-let [[b :as all] (seq s)]
        (when (or (Character/isLowerCase ^char b)
                  (every? #(Character/isUpperCase ^char %) all))
          (s/capitalize s)))
      s))

(defn hydrate-template [template context]
  (walk/postwalk (fn [x]
                   (if (param-key? x)
                     (if (contains? context x)
                       (get context x)
                       (throw (ex-info "Cannot find parameter value" {:missing-param x
                                                                      :available context})))
                     x))
                 template))

(defn inject-alias [df-patterns alias-symbol]
  (cond
    ;; (Dataflow-pattern) vector of 1-pattern
    (vector? df-patterns)
    (if (= 1 (count df-patterns))
      [(inject-alias (first df-patterns) alias-symbol)]
      (throw (ex-info "Expected only one element in dataflow-pattern vector"
                      {:dataflow-patterns df-patterns})))
    ;; (Dataflow-pattern) map
    (map? df-patterns)
    (assoc df-patterns
      :as alias-symbol)
    :else
    (throw (ex-info "Expected dataflow-patterns to be a vector or a map"
                    {:dataflow-patterns df-patterns
                     :type (type df-patterns)}))))

(defn parse-action-input-string [text]
  (output-parser/json-map-cascading-parser text))

(defn resolve-action-input [action-input-string]
  (let [string-between (fn [text prefix suffix]
                         (when (and (s/starts-with? text prefix)
                                    (s/ends-with? text suffix))
                           (let [text-len (count text)
                                 prefix-len (count prefix)
                                 suffix-len (count suffix)]
                             (when (>= text-len (+ prefix-len suffix-len))
                               (subs text prefix-len (- text-len suffix-len))))))
        source-&-offset (fn [text]
                          (let [match-offset-source (fn [pattern text])])
                          ;; FIXME: Handle referential text of form #"(.+) from previous result"
                          (if-let [[_ offset source] (re-matches #"(.+) from (.+) result" text)]
                            [source offset]
                            (if-let [[_ offset source] (re-matches #"(.+) from (.+)" text)]
                              [source offset]
                              (if-let [[_ offset source] (re-matches #"(.+)_from_(.+)" text)]
                                [source offset]
                                (if-let [[_ source offset] (re-matches #"(.+)\.(.+)" text)]
                                  [source offset]
                                  (if-let [[_ source offset] (re-matches #"(.+) (.+)" text)]
                                    [source offset]
                                    [nil text] #_"assume literal value"))))))
        process-value (fn [text-or-map]
                        (let [[text map-data] (if (map? text-or-map)
                                                [nil text-or-map]
                                                [text-or-map nil])]
                          (if map-data
                            map-data
                            (if (re-matches #".+@.+\..+" text) ; Email address?
                              text
                              (let [[clean-value
                                     placeholder?] (if-let [v (or (string-between text "<" ">")
                                                                  (string-between text "[" "]"))]
                                                     [v true]
                                                     [text false])
                                    [source offset] (source-&-offset clean-value)]
                                (if (some? source)          ; is it a reference?
                                  (->> offset
                                       maybe-capitalize
                                       (str source ".")
                                       keyword)
                                  (if placeholder?
                                    (keyword (str "%." offset))
                                    offset)))))))]
    (->> action-input-string
         parse-action-input-string
         (reduce-kv (fn [m input-name input-value]
                      (assoc m
                        (->> input-name
                             (str "param/")
                             keyword)
                        (process-value input-value)))
                    {}))))

(defn get-clean-alias-name [alias-name]
  (if (string? alias-name)
    (->> (s/split alias-name #",")
         (map s/trim)
         first
         keyword)
    (throw (ex-info "Expected a string alias-name"
                    {:alias-name alias-name}))))

(def placeholder-for-many :%)

(defn parse-action-qualifier [qualifier-text last-alias]
  (if-let [[_ each many] (re-matches #"\(for each (.+) in (.+)\)" qualifier-text)]
    {:type :for-each
     :args [each many]}
    (if-let [[_ each] (re-matches #"\(for each (.+)\)" qualifier-text)]
      {:type :for-each
       :args [each (or last-alias
                       (throw (ex-info "For-each construct supplied with invalid last-alias"
                                       {:for-each each
                                        :last-alias last-alias})))]}
      (throw (ex-info "Unknown action qualifier" {:qualifier-text qualifier-text})))))

(defn parse-action-name-and-qualifier [action-name-and-qualifier last-alias]
  (if (string? action-name-and-qualifier)
    (if-let [index (s/index-of action-name-and-qualifier " ")]
      [(subs action-name-and-qualifier 0 index)
       (-> action-name-and-qualifier
           (subs index)
           s/trim
           (parse-action-qualifier last-alias))]
      [action-name-and-qualifier
       nil])
    (throw (ex-info "Expected action-name-and-qualifier to be a string"
                    {:action-name action-name-and-qualifier}))))

(defn apply-action-qualifier [df-patterns action-qualifier alias-name]
  (if (nil? action-qualifier)
    df-patterns
    (if (vector? df-patterns)
      (let [{:keys [type args]} action-qualifier]
        (case type
          :for-each (let [[each many] args
                          prefix-qname (fn [prefix token]
                                         (let [string-token (name token)
                                               each-qualifier1 (str prefix ".") ; dot qualifier?
                                               each-qualifier2 (str prefix "_")]
                                           (when (nil? (namespace token))
                                             (cond
                                               ;; dot qualifier
                                               (s/starts-with? string-token each-qualifier1)
                                               (subs string-token (count each-qualifier1))
                                               ;; underscore qualifier
                                               (s/starts-with? string-token each-qualifier2)
                                               (maybe-capitalize (subs string-token (count each-qualifier2)))))))
                          each-qnamed (fn [token]
                                        (when (keyword? token)
                                          (if (= each (namespace token)) ; namespace qualifier?
                                            (name token)
                                            (or (prefix-qname each token)
                                                (prefix-qname (str "each " each "'s") token)))))]
                      (->> df-patterns
                           (walk/postwalk (fn [form]
                                            (if-let [qname (each-qnamed form)]
                                              (keyword (str "%." qname))
                                              form)))
                           (concat [:for-each (let [k (keyword many)]
                                                (if (= placeholder-for-many k)
                                                  alias-name
                                                  k))])
                           vec
                           vector))
          (throw (ex-info "Unknwon action qualifier" action-qualifier))))
      (throw (ex-info "Expected dataflow pattrns to be a vector"
                      {:dataflow-patterns df-patterns})))))

(defn resolve-action-step [action-step tools context last-alias]
  (let [{:keys [action-input                                ; JSON K/V pair
                action-result]
         action-name-and-qualifier :action-name} action-step
        _ (when (= "[]" action-name-and-qualifier)
            (throw (ex-info "Empty action-name" {:action-name action-name-and-qualifier})))
        [action-name
         action-qualifier] (parse-action-name-and-qualifier action-name-and-qualifier last-alias)
        tool-details (or (get tools action-name)
                         (throw (ex-info (format "Action name '%s' not found in tools" action-name)
                                         {:available-tools (keys tools)})))
        tool-param-names (->> (:params tool-details)
                              (map :name))
        df-patterns (:df-patterns tool-details)
        input-kv (resolve-action-input action-input)
        final-context (merge context input-kv)
        alias-name (get-clean-alias-name action-result)]
    ;; hydrate df-patterns with value
    [final-context
     alias-name
     (-> df-patterns
         (hydrate-template final-context)
         (inject-alias alias-name)
         (apply-action-qualifier action-qualifier alias-name))]))

(defn compose-datafow [plan tools]
  (log/debug (u/pretty-str "plan: " plan))
  (->> plan
       (reduce (fn [[context df-patterns last-alias] each-step]
                 (log/debug (str "Processing action-step: " each-step))
                 (let [[updated-context
                        alias-name
                        resolved-pattern] (resolve-action-step each-step tools context last-alias)]
                   (log/debug (str "Resolved action-step: " resolved-pattern))
                   [updated-context (-> df-patterns
                                        (concat resolved-pattern)
                                        vec)
                    alias-name]))
               [{} [] nil])
       second))
