(ns fractl.ui.util
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [cognitect.transit :as t]
            [fractl.util :as u]
            [fractl.global-state :as gs]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.ui.meta :as mt]
            [fractl.ui.config :as cfg]
            [fractl.ui.context :as ctx]))

(def ^:private remote-api-host (atom nil))
(def ^:private auth-required (atom false))
(def ^:private home-links (atom []))

(defn set-authorization-required! [auth-rec-name]
  (reset! auth-required auth-rec-name))

(defn authorized! []
  (reset! auth-required false))

(defn clear-home-links! []
  (reset! home-links []))

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
   :instance :Fractl.UI/RenderGenericInstanceForm
   :list :Fractl.UI/RenderGenericTable
   :dashboard :Fractl.UI/RenderGenericTable})

(defn- make-render-event [rec-name entity-spec tag meta]
  (let [spec-instance (:instance entity-spec)
        qinfo (:query-info entity-spec)
        qattrs (cond
                 spec-instance
                 {:Instance spec-instance}
                 qinfo
                 {:QueryBy (second qinfo)
                  :QueryValue (nth qinfo 2)}
                 :else {})
        tbl-attrs (case tag
                    (:list :dashboard)
                    {:Source (or (:source entity-spec)
                                 (lookupall-event-name rec-name))}
                    nil)
        app-config (gs/get-app-config)]
    (if-let [event-name (mt/view-event meta tag)]
      (cn/make-instance event-name (merge qattrs tbl-attrs))
      (let [attrs {:Record rec-name
                   :Fields (:order meta)}]
        (cn/make-instance
         (tag (or
               (get-in app-config [:ui :render-events rec-name])
               (get-in app-config [:ui :global-render-events])
               fallback-render-event-names))
         (merge attrs qattrs tbl-attrs))))))

(defn- make-view [tag target-info]
  (let [target-info (or @auth-required
                        (if (string? target-info)
                          (keyword target-info)
                          target-info))
        is-inst (cn/an-instance? target-info)
        is-raw-spec (and (not is-inst) (map? target-info))
        is-query-spec (and (not is-inst)
                           (not is-raw-spec)
                           (seqable? target-info))
        [rec-name final-entity-spec]
        (cond
          is-inst [(cn/instance-name target-info)
                   {:instance target-info}]
          is-query-spec [(first target-info)
                         {:query-info target-info}]
          :else [(if is-raw-spec
                   (:record target-info)
                   target-info)
                 (when is-raw-spec target-info)])
        meta (cn/fetch-meta rec-name)
        input-form-event
        (make-render-event rec-name final-entity-spec tag meta)
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

(defn- generate-view
  ([display-tag component-name entity-spec]
   (let [cn (name component-name)
         s (if (s/starts-with? cn ":")
             (subs cn 1)
             cn)]
     [:div
      [:b (str s " / ") [:a {:href "#"} "Home"]]
      [:div {:id main-view-id}
       (make-view display-tag entity-spec)]]))
  ([display-tag entity-spec]
   (let [en (if (keyword? entity-spec)
              entity-spec
              (first entity-spec))]
     (generate-view
      display-tag
      (first (li/split-path en))
      entity-spec))))

(def generate-input-view (partial generate-view :input))
(def generate-list-view (partial generate-view :list))
(def generate-instance-view (partial generate-view :instance))
(def generate-dashboard-view (partial generate-view :dashboard))

(defn make-home-view
  ([title dashboard-entity]
   (if-let [auth-rec-name @auth-required]
     (generate-input-view auth-rec-name)
     (let [dv (generate-dashboard-view dashboard-entity)]
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

(defn- lookup-by-event-name [c n s]
  (keyword (str (name c) "/" (name n) "LookupBy" s)))

(defn- make-query-event [rec-name query-by query-value]
  (let [[c n] (li/split-path rec-name)]
    (if (= :Id query-by)
      (cn/make-instance
       (keyword (str (name c) "/Lookup_" (name n)))
       {:Id query-value})
      (let [s (name query-by)]
        (cn/make-instance
         (lookup-by-event-name c n s)
         {query-by query-value})))))

(defn make-multi-arg-query-event [rec-name params]
  (let [[c n] (li/split-path rec-name)
        sfx (s/join "And" (mapv name (take-nth 2 params)))
        event-name (lookup-by-event-name c n sfx)]
    (cn/make-instance event-name (apply hash-map params))))

(defn make-multi-arg-query-event-spec [rec-name params]
  {:record rec-name
   :source (make-multi-arg-query-event rec-name params)})

(defn query-instance
  ([rec-name query-by query-value callback]
   (let [event-inst (make-query-event rec-name query-by query-value)]
     (eval-event
      (fn [r]
        (if-let [result (eval-result r)]
          (let [inst (first result)]
            (callback inst))
          (do (u/throw-ex
               (str "error: query-instance failed for "
                    [rec-name query-by query-value]
                    " - " r))
              (callback nil))))
      event-inst)))
  ([trigger-inst callback]
   (query-instance
    (:Record trigger-inst)
    (:QueryBy trigger-inst)
    (:QueryValue trigger-inst)
    callback)))

(defn- query-and-make-list-view [instance ref-rec-name
                                 [ref-attr refs]]
  (let [qevent (make-query-event ref-rec-name ref-attr (get-in instance refs))
        target-id (str "list-" (name ref-rec-name))
        v (make-list-view {:record ref-rec-name :source qevent})]
    `[:div {:id ~target-id} ~v]))

(defn- ref-to-record [rec-name attr-scms]
  (let [n (li/split-path rec-name)]
    (first
     (filter
      identity
      (map
       #(let [sname (second %)
              parts (:ref (cn/find-attribute-schema sname))]
          (when (and parts (= n [(:component parts) (:record parts)]))
            [(first %) (:refs parts)]))
       attr-scms)))))

(defn make-list-refs-view
  ([rec-name instance meta]
   (when-let [lrs (mt/contains meta)]
     (mapv
      #(when-let [scms (seq (cn/ref-attribute-schemas (cn/fetch-schema %)))]
         (when-let [r (ref-to-record rec-name scms)]
           (query-and-make-list-view instance % r)))
      lrs)))
  ([instance]
   (let [n (cn/instance-name instance)]
     (make-list-refs-view n instance (cn/fetch-meta n)))))
