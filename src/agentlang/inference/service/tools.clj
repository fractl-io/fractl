(ns agentlang.inference.service.tools
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.util.logger :as log]
            [agentlang.component :as cn]
            [agentlang.datafmt.json :as json]
            [agentlang.lang.raw :as raw]
            [agentlang.lang.internal :as li]
            [agentlang.lang.kernel :as k]))

(defn- record-name-as-function-name [rec-name]
  (let [rec-name (li/make-path rec-name)]
    (s/replace (s/replace (subs (str rec-name) 1) "." "__p__") "/" "__")))

(defn- function-name-as-record-name [fname]
  (keyword (s/replace (s/replace fname "__p__" ".") "__" "/")))

(def ^:private string-types #{"uuid" "datetime" "email" "date" "time"})
(def ^:private number-types #{"double" "float" "decimal" "int" "int64" "biginteger"})

(defn- find-root-type [attr-type]
  (let [s
        (case attr-type
          (:Now :Identity) "string"
          (cond
            (k/plain-kernel-type? attr-type)
            (s/lower-case (name attr-type))

            (k/kernel-type? attr-type)
            (s/lower-case (name (second (li/split-path attr-type))))

            (cn/find-schema attr-type) "object"

            :else nil))]
    ;; TODO: check the root-type keyword and return the appropriate type.
    ;; Do not compare strings.
    (cond
      (some #{s} string-types) "string"
      (some #{s} number-types) "number"
      :else s)))

(defn- as-tool-type [attr-type]
  (let [is-map (map? attr-type)
        spec
        (if is-map
          (if-let [xs (:oneof attr-type)]
            (cond
              (or (string? (first xs)) (keyword? xs))
              {:type "string" :enum (mapv name xs)}

              (number? (first xs))
              {:type "number" :enum xs}

              :else (u/throw-ex (str "cannot handle enum type for: " attr-type)))
            (if (:listof attr-type)
              {:type "object"}
              {:type (find-root-type (:type attr-type))}))
          {:type (find-root-type attr-type)})
        required (cond
                   is-map
                   (not (or (:optional attr-type) (:default attr-type)))

                   (= :Identity attr-type)
                   false

                   :else true)]
    [spec required]))

(defn- attribute-to-property [event-name [attr-name attr-type]]
  (let [[tool-type required] (as-tool-type attr-type)]
    (when-not (:type tool-type)
      (u/throw-ex (str "cannot translate "
                       [attr-name attr-type] " of " event-name
                       " to an appropriate tool-type")))
    [(name attr-name) tool-type required]))

(defn- record-to-tool
  ([find-schema rec-name docstring]
   (if-let [scm (find-schema rec-name)]
    (let [tool-name (record-name-as-function-name rec-name)
          props (mapv (partial attribute-to-property rec-name) (dissoc scm :meta))]
      {:type "function"
       :function
       {:name tool-name
        :description (or docstring (cn/docstring rec-name) tool-name)
        :parameters
        {:type "object"
         :properties (into {} (mapv (comp vec (partial take 2)) props))
         :required (vec (mapv first (filter last props)))}}})
    (log/warn (str "cannot generate tool, no schema found for - " rec-name))))
  ([find-schema rec-name] (record-to-tool find-schema rec-name nil)))

(def event-to-tool (partial record-to-tool raw/find-event))

(defn entity-to-tool [entity-name]
  (record-to-tool raw/find-entity entity-name (str "Create an instance of " entity-name)))

(defn all-tools-for-component [component]
  (let [event-tools (mapv event-to-tool (cn/event-names component))
        entity-tools (mapv entity-to-tool (cn/entity-names component))]
    (vec (us/nonils (concat event-tools entity-tools)))))

(defn- maybe-dissoc-attributes-with-defaults [recname attrs]
  (if-let [scm (or (raw/find-event recname) (raw/find-entity recname))]
    (if-let [anames (seq (map first (filter (fn [[k v]] (or (= v :Now) (= v :Identity))) scm)))]
      (apply dissoc attrs anames)
      attrs)
    attrs))

(defn tool-call-to-pattern [tool-call]
  (if-let [{fname "name" args "arguments"} (get tool-call "function")]
    (let [recname (function-name-as-record-name fname)
          attrs (maybe-dissoc-attributes-with-defaults recname (json/decode args))]
      {recname attrs})
    (u/throw-ex (str "Invalid tool-call: " tool-call))))
