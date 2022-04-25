(ns fractl.resolver.ui.input-form
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.ui.util :as vu]
            [fractl.relationship :as rel]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            ["@material-ui/core"
             :refer [TextField Card CardContent
                     Typography ButtonGroup Button
                     InputLabel Divider Select MenuItem
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(declare search-callback)

(defn- make-find-button [field-id get-state-value state-key search-event]
  (let [sid (str field-id "-search-pane")]
    [:div {:id sid}
     [:> Button
      {:on-click
       #(search-callback
         (cn/make-instance
          search-event
          {:S (str (get-state-value state-key) "%")})
         field-id sid
         (fn [] (make-find-button field-id get-state-value state-key search-event)))}
      "Find"]]))

(defn- search-callback [search-event-instance field-id target-id on-close-view]
  (vu/eval-event
   (fn [r]
     (if-let [results (vu/eval-result r)]
       (let [id (str field-id "-search-results")
             r (atom "")
             h (fn [v] (let [elem (-> js/document
                                      (.getElementById field-id))]
                         (set! (.-value elem) (first v))))
             target-elem #(-> js/document
                              (.getElementById target-id))
             rows (mapv (fn [inst]
                          (let [s (cn/instance-str inst)]
                            [:> MenuItem {:value [s (:Id inst)]} s]))
                        results)
             display-results
             (constantly
              `[:span
                [:> ~InputLabel {:id ~(str id "-label")} "Search Results"]
                [:> ~Select
                 {:label-id ~id
                  :id ~id
                  :label ~id
                  :on-change ~#(vu/call-with-value % h)}
                 ~@rows]
                [:> ~Button
                 {:on-click ~#(rdom/render [on-close-view] (target-elem))}
                 "Close"]])]
         (rdom/render [display-results] (target-elem)))
       (println (str "error: search failed - " r))))
   search-event-instance))

(defn- menu-items-from-rows [rows]
  (mapv
   (fn [r]
     [:> MenuItem {:value (:Id r)} (cn/instance-str r)])
   rows))

(defn- result-rows-to-select [sel-id handler rows]
  `[:> ~Select
    {:label-id ~(str sel-id "-label-id")
     :id ~sel-id
     :label ~sel-id
     :on-change ~handler}
    ~@(menu-items-from-rows rows)])

(defn- select-from-search [event-name sel-id handler target-id]
  (vu/eval-event
   (fn [r]
     (vu/render-view
      (if-let [rows (vu/eval-result r)]
        (result-rows-to-select sel-id handler rows)
        (do (println "error: failed to load data for " sel-id " - " r)
            [:span (str "failed to load data for " sel-id)]))
      target-id))
   (cn/make-instance
    {event-name {}})))

(defn- make-query-event [rec-name query-by query-value]
  (let [[c n] (li/split-path rec-name)]
    (if (= :Id query-by)
      (cn/make-instance
       (keyword (str (name c) "/Lookup_" (name n)))
       {:Id query-value})
      (cn/make-instance
       (keyword (str (name c) "/" (name n) "LookupBy" (name query-by)))
       {:S query-value}))))

(def ^:private instance-cell (atom nil))

(defn- query-instance [rec-name query-by query-value callback]
  (if-let [inst @instance-cell]
    (callback inst)
    (let [event-inst (make-query-event rec-name query-by query-value)]
      (vu/eval-event
       (fn [r]
         (if-let [result (vu/eval-result r)]
           (let [inst (first result)]
             (reset! instance-cell inst)
             (callback inst))
           (do (println
                (str "error: query-instance failed for "
                     [rec-name query-by query-value]
                     " - " r))
               (callback nil))))
       event-inst))))

(defn- set-value-cell! [rec-name field-id attr-name attr-scm query-spec]
  (let [[query-by query-value] query-spec
        elem (-> js/document
                 (.getElementById field-id))]
    (if (and query-by query-value)
      (query-instance
       rec-name query-by query-value
       (fn [inst]
         (set!
          (.-value elem)
          (str (attr-name inst)))))
      (set!
       (.-value elem)
       (str
        (when-let [d (:default attr-scm)]
          (if (fn? d) (d) d)))))))

(defn- render-attribute-specs [rec-name schema meta
                               fields custom-view-fns
                               query-spec get-state-value
                               change-handler]
  (let [fields (or fields (cn/attribute-names schema))
        list-refs (:list meta)]
    (reset! instance-cell nil)
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
              k field-name
              h (partial change-handler k)]
          (vu/add-post-render-event!
           #(set-value-cell! rec-name id field-name attr-scm query-spec))
          [:> TableRow
           [:> TableCell
            (if-let [view-fn
                     (when custom-view-fns
                       (field-name custom-view-fns))]
              (view-fn field-name attr-scm id h)
              (if-let [search-event (field-name list-refs)]
                (let [div-id (str n "-select")]
                  (select-from-search search-event id h div-id)
                  [:div {:id div-id}])
                [:> TextField
                 (merge
                  {:id id
                   :label n
                   :variant "standard"
                   :on-change h}
                  (when (cn/hashed-attribute? attr-scm)
                    {:type "password"}))]))]]))
      fields))))

(defn- render-table [rec-name table-view-id]
  (vu/render-view
   (vu/make-view rec-name :list)
   table-view-id))

(defn- upsert-callback [rec-name table-view-id result]
  (if (vu/eval-result result)
    (render-table rec-name table-view-id)
    (println (str "error: upsert failed for " rec-name " - " result))))

(defn- eval-event-callback [event-name on-success result]
  (if-let [r (vu/eval-result result)]
    (on-success r)
    (println (str "error: eval-event failed for " event-name " - " result))))

(defn- make-eval-success-callback [event-name meta]
  (if (vu/meta-authorize? meta)
    (fn [r]
      (if r
        (do
          (println (str event-name " success - " r))
          (vu/authorized!)
          (vu/render-app-view
           (vu/make-home-view)))
        (println (str event-name " failed - " r))))
    (fn [r]
      (println (str "eval result for " event-name " - " r)))))

(defn- navigation-buttons [rels prev-rec-name]
  (mapv
   (fn [rel]
     (let [rname (rel/relationship-name rel)
           [_ r] (li/split-path rname)
           n (name r)]
       [:> Button
        {:on-click #(vu/render-view
                     (vu/make-view rname :input))}
        n]))
   rels))

(defn- filter-relationships-of [rec-name rel-graph]
  (filter #(rel/participation (rel/relationship-spec %) rec-name) rel-graph))

(defn- close-button []
  (when-let [v (vu/pop-view-stack)]
    [:> Button
     {:on-click
      #(vu/render-view v)}
     "Close"]))

(defn- upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        [c r] (li/split-path rec-name)
        rel-graph (:graph (rel/relationships c))
        rels (filter-relationships-of rec-name rel-graph)
        title (name r)
        inst-state (r/atom {})
        change-handler (partial vu/assoc-input-value inst-state)
        get-state-value (fn [k] (get @inst-state k))
        scm (cn/fetch-schema rec-name)
        transformer (vu/make-transformer rec-name)
        table-view-id (str r "-table-view-container")
        meta (cn/fetch-meta rec-name)
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
                (vu/custom-view-fns instance)
                [(:QueryBy instance) (:QueryValue instance)]
                get-state-value change-handler)
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
                      rec-name inst
                      (partial upsert-callback rec-name table-view-id))))}
              ~(or (get-in meta [:views :create-button :label]) "Create")]
             ~@(navigation-buttons rels rec-name)]
            ~(close-button)]
           [:div {:id ~table-view-id}]]]]
    (vu/finalize-view view instance)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
