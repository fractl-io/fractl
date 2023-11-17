(ns
 fractl.model.fractl.kernel.store
 (:require
  [clojure.set :as set]
  [fractl.lang.internal :as li]
  [fractl.util.seq :as su])
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
  :clj-import
  '[(:require
     [clojure.set :as set]
     [fractl.lang.internal :as li]
     [fractl.util.seq :as su])]})
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
(defn- keyword-or-string? [x] (or (string? x) (keyword? x)))
(def maybe-all-ks? (partial maybe-all? keyword-or-string?))
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
      (mapv (fn* [p1__284#] (p1__284# obj)) [:add :alter :rename])))]
   (every? map? xs)
   true)
  (maybe-all? rename-col? :rename obj)
  (maybe-all-ks? :drop obj)))
(def
 constraints-keys
 #{:unique :index :drop-unique :drop-index li/guid})
(defn-
 constraints-spec?
 [obj]
 (and
  (map? obj)
  (= constraints-keys (set/union (set (keys obj)) constraints-keys))
  (if-let [ident (li/guid obj)] (keyword? ident) true)
  (maybe-all-ks? :index obj)
  (maybe-all-ks? :unique obj)
  (maybe-all-ks? :drop-unique obj)
  (maybe-all-ks? :drop-index obj)))
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
 "3d41072c-13d0-4337-a1a9-4124f41d64ed")
