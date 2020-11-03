(ns fractl.compiler.validation
  "Compile-time schema and data validation."
  (:require [clojure.set :as set]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.namespace :as n]))

(defn find-schema
  ([path orig-name]
   (if-let [tag-scm (n/find-schema path)]
     tag-scm
     (u/throw-ex (str "schema not found - " orig-name))))
  ([path] (find-schema path path)))

(defn invalid-attributes
  "Return the set of attributes in the pattern, that is not in the
  original schema. Return nil if there is no difference."
  [pattern-attrs schema]
  (let [orig-attrs (n/attribute-names schema)
        pattrs (set (map li/normalize-attr-name (keys pattern-attrs)))]
    (seq (set/difference pattrs orig-attrs))))

(def validate-attribute-value n/validate-attribute-value)

(defn validate-references [rec-name refs]
  (loop [k rec-name, rs refs]
    (when-let [r (first rs)]
      (let [[_ scm] (find-schema k)
            attrs (n/attributes scm)]
        (when-not (n/inferred-event-schema? scm)
          (if-let [adef (get attrs r)]
            (recur (:type adef) (rest rs))
            (u/throw-ex (str "invalid reference - " [rec-name refs])))))))
  refs)
