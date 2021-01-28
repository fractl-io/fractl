(ns fractl.resolver.ui
  (:require [clojure.walk :as w]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.lang.internal :as li]
            [fractl.env :as env]))

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

(defn- rewrite-names [env inst obj]
  (w/postwalk #(if (li/name? %)
                 (lookup-name env inst %)
                 %)
              obj))

(defn- preprocess [{env :env insts :insts}]
  (map
   #(if-let [v (:View %)]
      (assoc % :View (rewrite-names env inst v))
      %)
   insts))

(defn- upsert [inst]
  (let [target (:DOM_Target inst)
        view-fn (parse-view (:View inst))]
    (rgdom/render
     [view-fn]
     (.getElementById js/document target))))

(defn make-resolver [n]
  {:name n
   :upsert upsert
   :preprocess preprocess})
