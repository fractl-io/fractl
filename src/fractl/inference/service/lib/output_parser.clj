(ns fractl.inference.service.lib.output-parser
  "Output parsers and other output handlers"
  (:import (com.fasterxml.jackson.core JsonParseException))
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [cheshire.core :as json]
            [instaparse.core :as insta]
            [instaparse.transform :as instf]))

(defn json-parser [output-text]
  (json/parse-string output-text))

(def grammar-relaxed-json-map-parser
  (insta/parser
    "
    map = <'{'> key-value (<','> key-value)* <'}'>
    key-value = literal <':'> literal-or-symbol
    literal-or-symbol = literal | symbol
    literal = string | number | boolean | 'null'
    string = #'\"[^\"]*\"'
    number = #'[+-]?([0-9]*.)?[0-9]+'
    boolean = 'true' | 'false'
    symbol = #'[a-zA-Z_][a-zA-Z0-9._]*'
    "
    :auto-whitespace :standard))

(defn relaxed-json-map-parser
  "Parse a JSON map, with an exception that non-string references, e.g. foo.bar (as
  opposed to \"foo.bar\") are parsed as string."
  [output-text]
  (->> output-text
       (insta/parse grammar-relaxed-json-map-parser)
       (instf/transform {:string edn/read-string
                         :number edn/read-string
                         :boolean edn/read-string
                         :symbol (comp str edn/read-string) ; coerce symbol as string
                         :literal identity
                         :literal-or-symbol identity
                         :key-value vector
                         :map (comp (partial into {})
                                    vector)})))

(defn char-count [chars s]
  (-> (set chars)
      (filter s)
      count))

(defn json-map-cascading-parser
  "Parse a JSON map string. First try the regular parser, and if that fails then try a
  relaxed parser.
  See: relaxed-json-map-parser"
  [output-text]
  (try
    (json/parse-string output-text)
    (catch JsonParseException e
      (let [msg (.getMessage e)]
        (if (s/starts-with? msg "Unrecognized token")
          (->> output-text
               relaxed-json-map-parser)
          (let [left-bracket-count (char-count "{" output-text)
                right-bracket-count (char-count "}" output-text)]
            ;; check for missing trailing bracket
            (if (= left-bracket-count (inc right-bracket-count))
              (json/parse-string (str output-text "}"))
              (throw e))))))))

(defn llm-plan-parser
  "Given LLM plan-text output, parse it as a collection of action maps structured as follows:
  {:action-name   \"...\"
   :action-input  \",,,\"
   :action-result \"...\"}
  See: fractl.inference.service.lib.prompt/planner-template"
  {:type :llm-output-parser}
  [text]
  (let [action-count (atom 0)
        action-line? (fn [line] (s/starts-with? line "Action:"))
        after-leader (fn [text token]
                       (when (and text (s/starts-with? text token))
                         (s/trim (subs text (count token)))))
        get-action-line (fn [line] (after-leader line "Action:"))
        get-action-input (fn [line] (after-leader line "Action Input:"))
        get-action-result (fn [line] (after-leader line "Action Result:"))]
    (->> (s/split-lines text)
         (partition-by (fn [line]
                         (if (action-line? line)
                           (swap! action-count inc)
                           @action-count)))
         (filter (fn [lines] (action-line? (first lines))))
         (mapv (fn [[line-1
                     line-2
                     line-3
                     :as lines]]
                 (let [action-name (get-action-line line-1)
                       action-input (get-action-input line-2)
                       action-result (get-action-result line-3)]
                   (if (and action-name
                            action-input
                            action-result)
                     {:action-name   action-name
                      :action-input  action-input
                      :action-result action-result}
                     (throw (ex-info "Error parsing action name/input/result"
                                     {:lines [line-1
                                              line-2
                                              line-3]})))))))))
