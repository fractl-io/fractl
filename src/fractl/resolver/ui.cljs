(ns fractl.resolver.ui
  (:require [clojure.walk :as w]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.env :as env]))

(def view-tag :DOM_View)
(def target-tag :DOM_Target)

(defn- lookup-name [env inst path]
  (let [{c :component r :record refs :refs p :path} (li/path-parts path)]
    (cond
      (= p path)
      (path inst)

      (seq refs)
      (if p
        (get-in (p inst) refs)
        (let [[_ v] (env/instance-ref-path env [c r] nil refs)]
          v))

      :else (env/lookup-instance env [c r]))))

(defn- ui-component? [x]
  (and (cn/an-instance? x) (view-tag x)))

(declare preprocess-inst)

(defn- rewrite-names [env inst obj]
  (w/postwalk #(if (li/name? %)
                 (let [obj (lookup-name env inst %)]
                   (if (ui-component? obj)
                     (view-tag (preprocess-inst env obj))
                     obj))
                 %)
              obj))

(defn- preprocess-inst [env inst]
  (if-let [v (view-tag inst)]
    (assoc inst view-tag (rewrite-names env inst v))
    inst))

(defn- preprocess [{env :env insts :insts}]
  (map (partial preprocess-inst env) insts))

(defn- upsert [insts]
  (doseq [inst insts]
    (when-let [target (target-tag inst)]
      (rgdom/render
       [(fn [] (view-tag inst))]
       (.getElementById js/document target)))))

(defn make-resolver [n]
  {:name n
   :upsert upsert
   :preprocess preprocess})
