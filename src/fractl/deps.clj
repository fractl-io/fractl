(ns fractl.deps
  "Component dependency management."
  (:require [cemerick.pomegranate :refer [add-dependencies]]
            [fractl.lang.internal :as li]
            [fractl.util :as u]
            [fractl.lang.loader :as loader]
            [clojure.tools.namespace.track :as track])
  (:import (clojure.lang DynamicClassLoader)))

(defn- ensure-dynamic-classloader!
  []
  (let [thread (Thread/currentThread)
        context-class-loader (.getContextClassLoader thread)
        compiler-class-loader (.getClassLoader clojure.lang.Compiler)]
    (when-not (instance? DynamicClassLoader context-class-loader)
      (.setContextClassLoader
       thread (DynamicClassLoader. (or context-class-loader
                                       compiler-class-loader))))))

(defn- is-ns-exp
  [exp]
  (= (first exp) (symbol "component")))

(defn- deps-from-nsexp
  [componentexp]
  (let [nsname (li/validate-name (first (rest componentexp)))
        imps (rest (rest componentexp))]
    (if (not-empty imps)
      (if-let [fimps (li/validate-imports (first imps))]
        {nsname (into '#{} (map #(first %) (rest fimps)))}
        (u/throw-ex (str "Failed to parse imports for component " nsname)))
      {nsname '#{}})))

(defn- read-component-ns
  "A namespace in a fractl project is compiled into a clj namespace
      <component_name>.<flns>.clj

   This namespace is expected to contain an symbol 'expression' which
   simply embeds the entire fractl namespace code into a vector.
   A boilerplate clojure namespace expression added at the head of the list.
   Example fractl namespace:

     (component :Sample.Simple)

     (entity {:E1 {:X :Kernel/Int}})
     (entity {:E2 {:X :Kernel/Int}})

     ...
   This is compiled into:
        (ns sample.simple
          (:gen-class))

        (def expressions
        '[
        (component :Sample.Simple)

        (entity {:E1 {:X :Kernel/Int}})
        (entity {:E2 {:X :Kernel/Int}})

        ...
        ])
  "
  [cmns]
  (require (symbol cmns))
  (let [exps (eval (symbol (str (symbol cmns) "/" (symbol "expressions"))))
        modelexp (first (filter is-ns-exp exps))]
    (deps-from-nsexp modelexp)))

(defn- read-component-nslist
  "Each compiled fractl project is expected to contain a namespace called
      <component_name>.component.clj
   This namespace is expected to contain an symbol 'namespaces' which
   should correspond to a list of namespaces in the component that need to
   be loaded.
   Example:
       (ns sample.component
         (:gen-class))

       (def namespaces
         '[...])
  "
  [cetans]
  (require (symbol cetans))
  (eval (symbol (str (symbol cetans) "/" (symbol "namespaces")))))

(defn- load-component-ns [cns]
  (require (symbol cns))
  (let [exps (eval (symbol (str (symbol cns) "/" (symbol "expressions"))))]
    (loader/load-expressions (li/cns->component-name cns) exps)))

(defn- flnslist->cljnslist
  [nslist]
  (if-let [nsdepmap (reduce conj (into '[] (map read-component-ns nslist)))]
    (if-let [t (track/add '{} nsdepmap)]
      (let [loadnslist (::track/load t)]
        (map li/flns-as-cljns loadnslist))
      '())
    '()))

(defn component-nslist
  [cname cv]
  (ensure-dynamic-classloader!)
  (add-dependencies :coordinates (vec [(vec [(symbol cname) cv])]))
  (let [nslist (read-component-nslist (str cname ".component"))
        cljnslist (flnslist->cljnslist nslist)]
    (if-let [nsnames (into `[] (map load-component-ns cljnslist))]
      nsnames
      (u/throw-ex (str "Failed to load namespaces for component " cname)))))
