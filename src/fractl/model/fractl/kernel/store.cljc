(ns
 fractl.model.fractl.kernel.store
 (:require [clojure.set :as set])
 (:use
  [fractl.model.fractl.kernel.lang
   :only
   [Fractl_Kernel_Lang___COMPONENT_ID__]]
  [fractl.lang
   :only
   [dataflow entity attribute relationship component event record]]))
(component
 :Fractl.Kernel.Store
 {:refer [:Fractl.Kernel.Lang],
  :clj-import '[(:require [clojure.set :as set])]})
(def attrs-changes #{:alter :drop :rename :add})
(defn-
 rename-col?
 [x]
 (and
  (map? x)
  (= 2 (count (keys x)))
  (keyword? (:from x))
  (keyword? (:to x))))
(defn-
 maybe-all?
 [predic k obj]
 (if-let [x (k obj)] (every? predic x) true))
(def maybe-all-keyword? (partial maybe-all? keyword?))
(defn-
 attributes-spec?
 [obj]
 (and
  (map? obj)
  (= attrs-changes (set/union (set (keys obj)) attrs-changes))
  (if-let
   [xs
    (seq
     (filter
      identity
      (mapv (fn* [p1__275#] (p1__275# obj)) [:add :alter :rename])))]
   (every? map? xs)
   true)
  (maybe-all? rename-col? :rename obj)
  (maybe-all-keyword? :drop obj)))
(def
 constraints-keys
 #{:unique :identity :index :drop-unique :drop-index})
(defn-
 constraints-spec?
 [obj]
 (and
  (map? obj)
  (= constraints-keys (set/union (set (keys obj)) constraints-keys))
  (if-let [ident (:identity obj)] (keyword? ident) true)
  (maybe-all-keyword? :index obj)
  (maybe-all-keyword? :unique obj)
  (maybe-all-keyword? :drop-unique obj)
  (maybe-all-keyword? :drop-index obj)))
(entity
 :Fractl.Kernel.Store/Changeset
 {:Entity :Path,
  :Attributes
  {:check fractl.model.fractl.kernel.store/attributes-spec?,
   :optional true},
  :Contains {:oneof [:add :drop :none], :default :none},
  :Operation {:oneof [:alter :drop :rename], :default :alter},
  :Constraints
  {:check fractl.model.fractl.kernel.store/constraints-spec?,
   :optional true},
  :NewName {:type :Path, :optional true}})
(def
 Fractl_Kernel_Store___COMPONENT_ID__
 "0360eff4-f061-421c-bafc-4ae404e10fef")
