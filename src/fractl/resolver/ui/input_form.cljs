(ns fractl.resolver.ui.input-form
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.meta :as mt]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            [fractl.ui.context :as ctx]
            [fractl.ui.style :as style]
            [fractl.ui.config :as cfg]
            [fractl.relationship :as rel]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            ["@material-ui/core"
             :refer [TextField Card CardContent
                     TextareaAutosize
                     Typography Button
                     InputLabel Divider
                     Select MenuItem
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(defn- maybe-load-ref-from-context [attr-scm]
  (when-let [n (:ref attr-scm)]
    (let [{c :component r :record rs :refs} n
          inst (ctx/lookup-by-name [c r])]
      (when inst
        (if (= rs [cn/id-attr])
          [(cn/id-attr inst) (cn/instance-str inst)]
          [(get-in inst rs) nil])))))

(defn- fetch-local-value [attr-scm]
  (or (maybe-load-ref-from-context attr-scm)
      (when-let [d (:default attr-scm)]
        [(if (fn? d) (d) d) nil])))

(defn- set-value-cell! [rec-name field-id attr-name attr-scm
                        query-spec-or-instance set-state-value!]
  (let [inst (when (map? query-spec-or-instance) query-spec-or-instance)
        [query-by query-value] (when-not inst query-spec-or-instance)
        has-q (and query-by query-value)
        elem (-> js/document
                 (.getElementById field-id))
        cb (when (or inst has-q)
             (fn [inst]
               (let [v (attr-name inst)]
                 (when elem (set! (.-value elem) (str v)))
                 (set-state-value! attr-name v))))]
    (cond
      inst (cb inst)
      has-q (vu/query-instance
             rec-name query-by
             query-value cb)
      :else
      (when elem
        (set!
         (.-value elem)
         (let [[v s] (fetch-local-value attr-scm)]
           (set-state-value! attr-name v)
           s))))))

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

(defn- render-attribute-specs [rec-name schema
                               fields query-spec-or-instance
                               set-state-value!
                               change-handler]
  (let [fields (or fields (cn/attribute-names schema))
        inst (when (map? query-spec-or-instance) query-spec-or-instance)]
    (interpose
     [:> TableContainer
      [:> Table
       [:> TableBody
        [:> Divider]]]]
     (mapv
      (fn [arg]
        (let [field-name arg
              n (vu/display-name field-name)
              id (str "attribute-" n)
              attr-scm (cn/find-attribute-schema (field-name schema))
              h (partial change-handler field-name)
              [local-val s] (if-let [v (field-name inst)]
                              [(if (= cn/id-attr field-name)
                                 (cn/instance-str inst)
                                 v) nil]
                              (fetch-local-value attr-scm))
              is-required (not (:optional attr-scm))]
          (when local-val
            (set-state-value! field-name local-val))
          (when-not inst
            (vu/add-post-render-event!
             #(set-value-cell!
               rec-name id field-name attr-scm query-spec-or-instance
               set-state-value!)))
          [:> TableRow
           [:> TableCell
            (if-let [view-spec (cfg/views-attribute-view-spec rec-name field-name)]
              (process-attribute-view-spec
               view-spec {:id id :default-value local-val :on-change h})
              [:> TextField
               (merge
                {:id id
                 :label (if is-required (str n " *") n)
                 :default-value (or (or s local-val) "")
                 :variant "standard"
                 :on-change h}
                (when (or s (= cn/id-attr field-name))
                  {:disabled true})
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

(defn- make-eval-success-callback [event-name]
  (if (cfg/views-authorize? event-name)
    (fn [r]
      (if r
        (do
          (vu/authorized!)
          (ctx/attach-to-context! (first r) true)
          (v/render-home-view))
        (do (v/render-home-view [:div "login failed"])
            (u/throw-ex (str event-name " failed - " r)))))
    (fn [r]
      (let [rs (mapv fetch-upsert-result-inst r)]
        (v/render-view
         (v/make-list-view
          {:record (cn/instance-type (first rs))
           :source rs}))))))

(defn- navigation-buttons [rels prev-rec-name]
  (mapv
   (fn [rel]
     (let [rname (rel/relationship-name rel)
           [_ r] (li/split-path rname)
           n (vu/display-name r)]
       [:> Button
        {:on-click #(v/render-view
                     (v/make-input-view rname))}
        n]))
   rels))

(defn- filter-relationships-of [rec-name rel-graph]
  (filter #(rel/participation (rel/relationship-spec %) rec-name) rel-graph))

(defn- close-button []
  (when-let [v (vu/pop-view-stack)]
    [:> Button
     {:on-click
      #(v/render-view v)}
     "Close"]))

(def ^:private error-msg-id "error-msg")

(defn- validate-inst-state [inst-state schema]
  (let [anames (cn/attribute-names schema)]
    (doseq [n anames]
      (let [ascm (cn/find-attribute-schema (n schema))]
        (when-not (:optional ascm)
          (when-not (seq (n inst-state))
            (let [msg (str (name n) " is required")]
              (js/alert msg)
              (u/throw-ex msg))))))
    inst-state))

(defn upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        [c r] (li/split-path rec-name)
        rel-graph (:graph (rel/relationships c))
        rels (filter-relationships-of rec-name rel-graph)
        title (vu/display-name r)
        inst-state (r/atom {})
        change-handler (partial vu/assoc-input-value inst-state)
        get-state-value (fn [k] (get @inst-state k))
        set-state-value! (fn [k v] (swap! inst-state assoc k (str v)))
        scm (cn/fetch-schema rec-name)
        transformer (vu/make-transformer rec-name)
        meta (cn/fetch-meta rec-name)
        embedded-inst (:Instance instance)
        styles (cfg/views-styles rec-name)
        view
        `[:div {:class "view"}
           [:> ~Card ~(style/input-form-card styles)
            [:> ~CardContent {:style {:padding "20px" :text-align "center"}}
             [:> ~Typography  ~(style/input-form-title styles)
              ~title][:br]
             ~@(render-attribute-specs
                rec-name scm
                 (mapv
                  u/string-as-keyword
                  (:Fields instance))
                 (or embedded-inst [(:QueryBy instance) (:QueryValue instance)])
                 set-state-value! change-handler)
             [:> ~Button
              {:variant "contained"
                  :style {:margin-top "10px" :background "lightblue" }
                :on-click
               ~#(let [inst (transformer (validate-inst-state @inst-state scm))]
                   (if (cn/event? rec-name)
                     (vu/eval-event
                      (partial
                       eval-event-callback
                       rec-name (make-eval-success-callback rec-name))
                      inst)
                     (vu/fire-upsert
                      rec-name inst (mt/upsert-event meta)
                      (partial upsert-callback rec-name))))}
              ~(if embedded-inst "Save" (or (cfg/views-create-button-label rec-name) "Create"))]
             ~@(navigation-buttons rels rec-name)]
            ;; [:div "(* = required)"]
            ~@(when embedded-inst
                (v/make-list-refs-view rec-name embedded-inst meta))
            ~(close-button)]]]
    (vu/finalize-view view instance)))

(defn make [resolver-name]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
