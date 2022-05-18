(ns fractl.resolver.ui.input-form
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            [fractl.ui.context :as ctx]
            [fractl.ui.meta :as mt]
            [fractl.relationship :as rel]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            ["@material-ui/core"
             :refer [TextField Card CardContent
                     TextareaAutosize
                     Typography ButtonGroup Button
                     InputLabel Divider Select MenuItem
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(def ^:private instance-cache (atom nil))

(defn- maybe-load-ref-from-context [attr-scm]
  (when-let [n (:ref attr-scm)]
    (let [{c :component r :record rs :refs} n]
      (ctx/lookup-ref [c r] rs))))

(defn- fetch-local-value [set-state-value! attr-name attr-scm]
  (let [v (str
           (if-let [d (:default attr-scm)]
             (if (fn? d) (d) d)
             (maybe-load-ref-from-context attr-scm)))]
    (set-state-value! attr-name v)
    v))

(defn- set-value-cell! [rec-name field-id attr-name attr-scm
                        query-spec-or-instance set-state-value!]
  (let [inst (when (map? query-spec-or-instance) query-spec-or-instance)
        cached-inst (when-not inst @instance-cache)
        [query-by query-value] (when-not (or cached-inst inst) query-spec-or-instance)
        has-q (and query-by query-value)
        elem (-> js/document
                 (.getElementById field-id))
        cb (when (or inst has-q)
             (fn [inst]
               (let [v (str (attr-name inst))]
                 (when elem (set! (.-value elem) v))
                 (set-state-value! attr-name v))))]
    (cond
      inst (cb inst)
      cached-inst (cb cached-inst)
      has-q (vu/query-instance
             rec-name query-by
             query-value cb)
      :else
      (when elem
        (set!
         (.-value elem)
         (fetch-local-value
          set-state-value! attr-name
          attr-scm))))))

(defn- keyword-as-ui-component [k]
  (case k
    :TextField TextField
    :TextareaAutosize TextareaAutosize
    :Button Button
    :InputLabel InputLabel
    :Select Select
    :MenuItem MenuItem
    nil))

(defn- process-attribute-view-spec [view-spec props]
  (if-let [ui-comp (keyword-as-ui-component (second view-spec))]
    [:> ui-comp (merge (nth view-spec 2) props)]
    (u/throw-ex (str "no ui component for " (second view-spec)))))

(defn- render-attribute-specs [rec-name schema meta
                               fields query-spec-or-instance
                               set-state-value!
                               change-handler]
  (let [fields (or fields (cn/attribute-names schema))
        inst (when (map? query-spec-or-instance) query-spec-or-instance)]
    (reset! instance-cache nil)
    (interpose
     [:> TableContainer
      [:> Table
       [:> TableBody
        [:> Divider]]]]
     (mapv
      (fn [arg]
        (let [field-name arg
              n (name field-name)
              id (str "attribute-" n)
              attr-scm (cn/find-attribute-schema (field-name schema))
              h (partial change-handler field-name)
              local-val (or (field-name inst)
                            (fetch-local-value set-state-value! field-name attr-scm))]
          (when local-val
            (set-state-value! field-name local-val))
          (when-not inst
            (vu/add-post-render-event!
             #(set-value-cell!
               rec-name id field-name attr-scm query-spec-or-instance
               set-state-value!)))
          [:> TableRow
           [:> TableCell
            (if-let [view-spec (mt/attribute-view-spec meta field-name)]
              (process-attribute-view-spec
               view-spec {:id id :default-value local-val :on-change h})
              [:> TextField
               (merge
                {:id id
                 :label n
                 :default-value (or local-val "")
                 :variant "standard"
                 :on-change h}
                (when (cn/hashed-attribute? attr-scm)
                  {:type "password"}))])]]))
      fields))))

(defn- fetch-upsert-result-inst [r]
  (if-let [t (:transition r)]
    (:to t)
    r))

(defn- upsert-callback [rec-name result]
  (if-let [r (vu/eval-result result)]
    (let [inst (first r)]
      (ctx/attach-to-context! inst)
      (v/render-view
       (v/make-instance-view (fetch-upsert-result-inst inst))))
    (let [s (str "error: upsert failed for " rec-name)]
      (v/render-view
       [:div s])
      (u/throw-ex (str  s " - " result)))))

(defn- eval-event-callback [event-name callback result]
  (callback (vu/eval-result result)))

(defn- make-eval-success-callback [event-name meta]
  (if (mt/authorize? meta)
    (fn [r]
      (if r
        (do
          (vu/authorized!)
          (ctx/attach-to-context! (first r) true)
          (v/render-home-view))
        (do (v/render-home-view [:div "login failed"])
            (u/throw-ex (str event-name " failed - " r)))))
    (fn [r]
      (println (str "eval result for " event-name " - " r)))))

(defn- navigation-buttons [rels prev-rec-name]
  (mapv
   (fn [rel]
     (let [rname (rel/relationship-name rel)
           [_ r] (li/split-path rname)
           n (name r)]
       [:> Button
        {:on-click #(v/render-view
                     (v/make-input-view rname))}
        n]))
   rels))

(defn- filter-relationships-of [rec-name rel-graph]
  (filter #(rel/participation (rel/relationship-spec %) rec-name) rel-graph))

(defn- close-button []
  (when-let [v (v/pop-view-stack)]
    [:> Button
     {:on-click
      #(v/render-view v)}
     "Close"]))

(defn upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        [c r] (li/split-path rec-name)
        rel-graph (:graph (rel/relationships c))
        rels (filter-relationships-of rec-name rel-graph)
        title (name r)
        inst-state (r/atom {})
        change-handler (partial vu/assoc-input-value inst-state)
        get-state-value (fn [k] (get @inst-state k))
        set-state-value! (fn [k v] (swap! inst-state assoc k v))
        scm (cn/fetch-schema rec-name)
        transformer (vu/make-transformer rec-name)
        meta (cn/fetch-meta rec-name)
        embedded-inst (:Instance instance)
        view
        `[:div {:class "view"}
          [:div {:class "main"}
           [:> ~Card {:variant "outlined"}
            [:> ~CardContent
             [:> ~Typography {:gutterBottom true :variant "h5" :component "div"}
              ~title][:br]
             ~@(render-attribute-specs
                rec-name scm meta
                 (mapv
                  u/string-as-keyword
                  (:Fields instance))
                 (or embedded-inst [(:QueryBy instance) (:QueryValue instance)])
                 set-state-value! change-handler)
             [:> ~Button
              {:on-click
               ~#(let [inst (transformer @inst-state)]
                   (if (cn/event? rec-name)
                     (vu/eval-event
                      (partial
                       eval-event-callback
                       rec-name (make-eval-success-callback rec-name meta))
                      inst)
                     (vu/fire-upsert
                      rec-name inst (mt/upsert-event meta)
                      (partial upsert-callback rec-name))))}
              ~(if embedded-inst "Save" (or (mt/create-button-label meta) "Create"))]
             ~@(navigation-buttons rels rec-name)]
            ~@(when embedded-inst
                (v/make-list-refs-view rec-name embedded-inst meta))
            ~(close-button)]]]]
    (v/finalize-view view instance)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
