(ns fractl.ui.util
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [cognitect.transit :as t]
            [fractl.global-state :as gs]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.ui.config :as cfg]
            [fractl.ui.context :as ctx]))

(def ^:private remote-api-host (atom nil))
(def ^:private auth-required (atom false))
(def ^:private home-links (atom []))

(defn set-authorization-required! [auth-rec-name]
  (reset! auth-required auth-rec-name))

(defn authorized! []
  (reset! auth-required false))

(defn attach-home-link! [s]
  (swap! home-links conj s))

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
   (let [event-instance (assoc event-instance li/event-context
                               (ctx/context-as-map))]
     (if-let [host (and (not eval-local) @remote-api-host)]
       (do (ev/remote-evaluate host callback event-instance) nil)
       ((or callback identity) ((ev/global-dataflow-eval) event-instance)))))
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

(def ^:private s-lookup-all "LookupAll")

(defn lookupall-event-name [rec-name]
  (keyword
   (if (string? rec-name)
     (str rec-name s-lookup-all)
     (let [[c n] (li/split-path rec-name)]
       (str (name c) "/" (name n) s-lookup-all)))))

(def ^:private fallback-render-event-names
  {:input :Fractl.UI/RenderGenericInputForm
   :instance :Fractl.UI/RenderGenericDisplayForm
   :list :Fractl.UI/RenderGenericTable
   :dashboard :Fractl.UI/RenderGenericTable})

(defn- make-render-event [rec-name entity-spec
                          tag meta spec-has-query-info]
  (let [qattrs (if spec-has-query-info
                 (if (map? entity-spec)
                   {:Instance entity-spec}
                   {:QueryBy (second entity-spec)
                    :QueryValue (nth entity-spec 2)})
                 {})
        tbl-attrs (case tag
                    (:list :dashboard)
                    {:Source (lookupall-event-name rec-name)}
                    nil)
        app-config (gs/get-app-config)]
    (if-let [event-name (get-in meta [:views tag])]
      (cn/make-instance event-name (merge qattrs tbl-attrs))
      (let [attrs {:RecordName rec-name
                   :Fields (:order meta)}]
        (cn/make-instance
         (tag (or
               (get-in app-config [:ui :render-events rec-name])
               (get-in app-config [:ui :global-render-events])
               fallback-render-event-names))
         (merge attrs qattrs tbl-attrs))))))

(defn- make-view [tag entity-spec]
  (let [entity-spec (or @auth-required
                        (if (string? entity-spec)
                          (keyword entity-spec)
                          entity-spec))
        is-spec (seqable? entity-spec)
        rec-name (if is-spec
                   (first entity-spec)
                   entity-spec)
        meta (cn/fetch-meta rec-name)
        input-form-event
        (make-render-event
         rec-name entity-spec tag meta is-spec)
        r (eval-event nil true input-form-event)
        v (first (eval-result r))]
    (or (:View v)
        (do (println (str "input form generation failed. " r))
            [:div "failed to generate view for " [rec-name tag]]))))

(def make-instance-view (partial make-view :instance))
(def make-list-view (partial make-view :list))
(def make-input-view (partial make-view :input))

(def ^:private post-render-events (atom []))

(defn add-post-render-event! [event-fn]
  (swap! post-render-events conj event-fn))

(defn run-post-render-events! []
  (let [fns @post-render-events]
    (reset! post-render-events [])
    (doseq [f fns]
      (f))))

(def ^:private interval-handle (atom nil))

(defn clear-interval! []
  (when-let [h @interval-handle]
    (js/clearInterval h)))

(defn set-interval! [callback ms]
  (add-post-render-event!
   (fn []
     (clear-interval!)
     (reset!
      interval-handle
      (js/setInterval callback ms)))))

(defn reset-page-state! []
  (clear-interval!))

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
  (reset-page-state!)
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
       (make-view display-tag entity-spec)]]))
  ([entity-spec display-tag]
   (let [en (if (keyword? entity-spec)
              entity-spec
              (first entity-spec))]
     (generate-view
      (first (li/split-path en))
      entity-spec display-tag)))
  ([entity-spec]
   (generate-view entity-spec :input)))

(defn meta-authorize? [meta]
  (= :authorize
     (get-in
      meta
      [:views :create-button :on-success])))

(defn make-home-view
  ([title dashboard-entity]
   (if-let [auth-rec-name @auth-required]
     (generate-view auth-rec-name)
     (let [dv (generate-view dashboard-entity :dashboard)]
       `[:div [:h1 ~title]
         ~@(deref home-links)
         [:div ~dv]])))
  ([]
   (make-home-view "Dashboard" (cfg/dashboard))))

(def ^:private view-stack (atom []))

(defn pop-view-stack []
  (when-let [v (peek @view-stack)]
    (swap! view-stack pop)
    v))

(defn push-on-view-stack! [view]
  (swap! view-stack conj view))

(defn finalize-view [view event-instance]
  (push-on-view-stack! view)
  (assoc event-instance :View view))
