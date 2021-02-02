(ns fractl.resolver.ui
  (:require [clojure.walk :as w]
            [taoensso.timbre :as log]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.lang.internal :as li]
            [fractl.env :as env]
            [fractl.store.reagent.core :as rstore]))

(def view-tag :DOM_View)
(def target-tag :DOM_Target)

(defn- follow-ref [env inst rec-schema attr-name refs]
  (if-let [rp (:ref (cn/find-attribute-schema
                          (get rec-schema attr-name)))]
    (let [ukattr (first (:refs rp))
          rec-name [(:component rp) (:record rp)]
          ukattr-val (attr-name inst)]
      [:cursor (concat [rec-name ukattr-val] refs)])
    (u/throw-ex (str "invalid reference - " [attr-name refs]))))

(defn- lookup-reference [env inst rec-schema parts]
  (let [{c :component r :record
         refs :refs p :path} parts]
    (cond
      p
      (or (get-in (p inst) refs)
          (follow-ref env inst rec-schema p refs))

      (not c)
      (follow-ref env inst rec-schema r refs)

      :else
      (let [[_ v] (env/instance-ref-path env [c r] nil refs)]
        v))))

(defn- lookup-name [env inst rec-schema path]
  (let [{c :component r :record refs :refs p :path :as parts}
        (li/path-parts path)]
    (cond
      (= p path)
      (path inst)

      (seq refs)
      (lookup-reference env inst rec-schema parts)

      :else (env/lookup-instance env [c r]))))

(defn- ui-component? [x]
  (and (cn/an-instance? x) (view-tag x)))

(defn- rewrite-event [model-event spec]
  (let [with-args? (seqable? model-event)
        [n args]
        (if with-args?
          [(first model-event) (first (rest model-event))]
          [model-event nil])]
    [(first spec)
     (fn [event-obj]
       (if (= n :set)
         (reset! (first args)
                 (if (= :-value (second args))
                   (-> event-obj .-target .-value)
                   (second args)))
         (let [r (e/eval-all-dataflows
                  (cn/make-instance
                   {n {:EventObject event-obj
                       :UserData args}}))]
           (doall r))))]))

(defn- normalize-cursor [obj]
  (let [x (second obj)]
    (if (and (seqable? x) (= :cursor (first x)))
      [(first obj) (second (second x))]
      obj)))

(def ^:private ui-event-names #{:on-click :on-change})

(defn- maybe-rewrite-event [obj]
  (if (and (vector? obj)
           (some #{(first obj)} ui-event-names))
    (let [x (second obj)]
      (if (fn? x)
        obj
        (rewrite-event (normalize-cursor x) obj)))
    obj))

(defn- find-schema [inst]
  (let [n (cn/instance-name inst)]
    (or (cn/entity-schema n) (cn/record-schema n))))

(declare preprocess-inst)

(defn- rewrite-names [env rec-schema inst obj]
  (w/postwalk
   #(if (li/name? %)
      (let [obj (lookup-name env inst rec-schema %)]
        (if (ui-component? obj)
          (view-tag (preprocess-inst env (find-schema obj) obj))
          obj))
      (maybe-rewrite-event %))
   obj))

(defn- preprocess-inst [env rec-schema inst]
  (if-let [v (view-tag inst)]
    (assoc inst view-tag (rewrite-names env rec-schema inst v))
    inst))

(defn- preprocess [{env :env insts :insts}]
  (map (partial preprocess-inst env (find-schema (first insts))) insts))

(def ^:private cursors (atom {}))

(defn- fetch-cursor [path]
  (or (get @cursors path)
      (let [c (rg/cursor rstore/state path)]
        (swap! cursors assoc path c)
        c)))

(defn- process-cursors [spec]
  (w/postwalk
   #(if (and (seqable? %) (= :cursor (first %)))
      (deref (fetch-cursor (second %)))
      %)
   spec))

(defn- upsert [insts]
  (doseq [inst insts]
    (when-let [target (target-tag inst)]
      (rgdom/render
       [(fn [] (process-cursors (view-tag inst)))]
       (.getElementById js/document target)))))

(defn make-resolver [n]
  {:name n
   :upsert upsert
   :preprocess preprocess})
