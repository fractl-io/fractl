(ns fractl.ui.util
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [cognitect.transit :as t]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]))

(def ^:private remote-api-host (atom nil))

(defn set-remote-api-host! [host]
  (reset! remote-api-host host))

(defn get-remote-api-host []
  @remote-api-host)

(defn eval-result [result]
  (let [r (first result)]
    (if (= :ok (:status r))
      (:result r)
      (do (println "remote eval failed: " r) nil))))

(defn eval-event
  ([callback eval-local event-instance]
   (if-let [host (and (not eval-local) @remote-api-host)]
     (do (ev/remote-evaluate host callback event-instance) nil)
     ((or callback identity) ((ev/global-dataflow-eval) event-instance))))
  ([callback event-instance]
   (eval-event callback false event-instance))
  ([event-instance]
   (eval-event identity event-instance)))

(defn eval-local-event [event-instance]
  (eval-event identity true event-instance))

(defn- upsert-event-name [entity-name]
  (let [[c n] (li/split-path entity-name)
        ev-name (keyword (str "Upsert_" (name n)))]
    (li/make-path c ev-name)))

(defn fire-upsert
  ([entity-name object callback]
   (let [event-name (upsert-event-name entity-name)]
     (eval-event
      callback
      (cn/make-instance
       {event-name
        {:Instance
         (if (cn/an-instance? object)
           object
           (cn/make-instance
            {entity-name object}))}}))))
  ([entity-name object]
   (fire-upsert entity-name object identity)))

(defn make-transformer
  ([recname schema]
   (fn [instance]
     (let [inst1
           (mapv
            (fn [[k v]]
              [k
               (let [tn (k schema)
                     t (if (k/kernel-type? tn) tn (:type (cn/find-attribute-schema tn)))]
                 (case t
                   (:Kernel/Int :Kernel/Int64 :Kernel/BigInteger) (js/parseInt v)
                   (:Kernel/Float :Kernel/Decimal :Kernel/Double) (js/parseFloat v)
                   :Kernel/Boolean (if (= v "false") false true)
                   v))])
            instance)]
       (cn/make-instance
        {recname
         (into {} inst1)}))))
  ([recname]
   (make-transformer recname (cn/fetch-schema recname))))

(defn assoc-input-value [place k evt]
  (swap! place assoc k (-> evt .-target .-value)))

(defn call-with-value [evt callback]
  (callback (-> evt .-target .-value)))

(defn make-view [entity-spec tag]
  (let [is-spec (seqable? entity-spec)
        rec-name (if is-spec
                   (first entity-spec)
                   entity-spec)
        meta (cn/fetch-meta rec-name)
        input-form-event
        (cn/make-instance
         (get-in meta [:views tag])
         (if is-spec
           {:QueryBy (second entity-spec)
            :QueryValue (nth entity-spec 2)}
           {}))
        r (eval-event nil true input-form-event)
        v (first (eval-result r))]
    (or (:View v)
        (do (println (str "input form generation failed. " r))
            [:div "failed to generate view for " [rec-name tag]]))))

(def ^:private post-render-events (atom []))

(defn add-post-render-event! [event-fn]
  (swap! post-render-events conj event-fn))

(defn run-post-render-events! []
  (let [fns @post-render-events]
    (reset! post-render-events [])
    (doseq [f fns]
      (f))))

(def main-view-id "main-view")

(defn render-view
  ([view-spec elem-id]
   (rdom/render
    [(fn [] view-spec)]
    (-> js/document
        (.getElementById elem-id)))
   (run-post-render-events!))
  ([view-spec]
   (render-view view-spec main-view-id)))

(defn render-app-view [view-spec]
  (render-view view-spec "app"))

(defn decode-to-str [x]
  (if (t/tagged-value? x)
    (.-rep x)
    (str x)))

(defn generate-view
  ([component-name entity-spec display-tag]
   (let [cn (name component-name)
         s (if (s/starts-with? cn ":")
             (subs cn 1)
             cn)]
     [:div
      [:b (str s " / ") [:a {:href "#"} "Home"]]
      [:div {:id main-view-id}
       (make-view entity-spec display-tag)]]))
  ([entity-spec display-tag]
   (let [en (if (keyword? entity-spec)
              entity-spec
              (first entity-spec))]
     (generate-view
      (first (li/split-path en))
      entity-spec display-tag)))
  ([entity-spec]
   (generate-view entity-spec :input)))

(defn main-view
  ([render-fn root-entity display-tag]
   (let [[c _] (li/split-path root-entity)]
     (render-fn
      (fn []
        (generate-view c root-entity display-tag)))))
  ([render-fn root-entity]
   (main-view render-fn root-entity :input)))
