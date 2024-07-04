(ns fractl.inference.service.model
  (:require [clojure.java.io :as io]
            [fractl.lang :refer [component
                                 dataflow
                                 entity
                                 event
                                 record
                                 relationship]]
            [fractl.util :as u])
  (:import [java.net MalformedURLException]))

(component :Fractl.Inference.Service)

(defn url?
  "Return true if supplied string is a URL, false otherwise."
  [u]
  (try
    (and (io/as-url u)
         true)
    (catch MalformedURLException _
      false)))

(entity
 :Fractl.Inference.Service/DocChunk
 {:AppUuid {:type :UUID :default u/uuid-string}
  :DocName :String
  :DocChunk :Any})

(defn tool-spec-param-type? [t]
  (and (map? t)
       (string? (:type t))
       (if-let [f (:format t)]
         (string? f)
         true)))

(defn tool-spec-param? [p]
  (and (map? p)
       (string? (:name p))
       (tool-spec-param-type? (:type p))
       (boolean? (:required p))))

(defn tool-spec? [obj]
  (and (map? obj)
       (string? (:description obj))
       (every? tool-spec-param? (:params obj))
       (vector? (:df-patterns obj))))

(entity
 :Fractl.Inference.Service/PlannerTool
 {:AppUuid {:type :UUID :default u/uuid-string}
  :ToolName {:type :String :optional true}
  :ToolSpec {:check tool-spec?}
  :Tag :String
  :Type :String
  :MetaContent :String
  :Operation :String})

(record
 :Fractl.Inference.Service/QuestionOptions
 {:UseDocs {:type :Boolean :default true}
  :UseTools {:type :Boolean :default true}
  ;; tools related options (applicable if :UseTools is true)
  :Classification {:type :Boolean :default true}
  :ChainOfThought {:type :Boolean :default true}})

(entity
 :Fractl.Inference.Service/Question
 {:ChatUuid {:type :UUID :default u/uuid-string}
  :AppUuid :UUID
  :Question :String
  :QuestionContext {:type :Map :default {}}
  :QuestionOptions {:type :Map :default {}}
  :QuestionResponse {:type :Any :optional true :read-only true}})
