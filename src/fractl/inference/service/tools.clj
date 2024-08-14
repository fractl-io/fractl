(ns fractl.inference.service.tools
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.datafmt.json :as json]
            [fractl.lang.raw :as raw]
            [fractl.lang.internal :as li]
            [fractl.lang.kernel :as k]))

(defn- record-name-as-function-name [rec-name]
  (let [rec-name (li/make-path rec-name)]
    (s/replace (s/replace (subs (str rec-name) 1) "." "__p__") "/" "__")))

(defn- function-name-as-record-name [fname]
  (keyword (s/replace (s/replace fname "__p__" ".") "__" "/")))

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
      (or (= s "uuid") (= s "datetime")) "string"
      (or (= s "double") (= s "float") (= s "int")) "number"
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
    (u/pretty-trace (vec (us/nonils (concat event-tools entity-tools))))))

(defn tool-call-to-pattern [tool-call]
  (if-let [{fname "name" args "arguments"} (get tool-call "function")]
    {(function-name-as-record-name fname) (json/decode args)}
    (u/throw-ex (str "Invalid tool-call: " tool-call))))
