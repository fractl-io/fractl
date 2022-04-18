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

(defn- result-rows-to-select [sel-id value-cell handler rows]
  `[:> ~Select
    {:label-id ~(str sel-id "-label-id")
     :id ~sel-id
     :label ~sel-id
     :value @value-cell
     :on-change ~handler}
    ~@(menu-items-from-rows rows)])

(defn- select-from-search [event-name value-cell sel-id handler target-id]
  (vu/eval-event
   (fn [r]
     (vu/render-view
      (if-let [rows (vu/eval-result r)]
        (result-rows-to-select sel-id value-cell handler rows)
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
       (keyword (str (name c) "/Lookup" (name n) "By" (name query-by)))
       {:S query-value}))))

(def ^:private instance-cell (atom nil))

(defn- query-instance [rec-name query-by query-value callback]
  ;; TODO: debug query instance
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

(defn- set-value-cell! [rec-name cell attr-name attr-scm query-spec]
  (let [[query-by query-value] query-spec]
    (if (and query-by query-value)
      (attr-name
       (query-instance
        rec-name query-by query-value
        (fn [inst] (reset! cell (attr-name inst)))))
      (reset!
       cell
       (str
        (when-let [d (:default attr-scm)]
          (if (fn? d) (d) d)))))))

(defn- render-attribute-specs [rec-name schema fields query-spec
                               get-state-value change-handler]
  (let [fields (or fields (cn/attribute-names schema))
        list-refs (:list (cn/fetch-meta rec-name))]
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
              id n
              attr-scm (cn/find-attribute-schema (field-name schema))
              value-cell (r/atom "")
              k field-name
              h (partial change-handler k)]
          (set-value-cell! rec-name value-cell field-name attr-scm query-spec)
          [:> TableRow
           [:> TableCell
            (if-let [search-event (field-name list-refs)]
              (let [div-id (str n "-select")]
                (select-from-search search-event value-cell id h div-id)
                [:div {:id div-id}])
              [:> TextField
               {:id id
                :label n
                :default-value @value-cell
                :variant "standard"
                :on-change h}])]]))
      fields))))

(defn- upsert-callback [rec-name table-view-id result]
  (if (vu/eval-result result)
    (vu/render-view
     (vu/make-view rec-name :list)
     table-view-id)
    (println (str "error: upsert failed for " rec-name " - " result))))

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

(def ^:private view-stack (atom []))

(defn- close-button []
  (let [s @view-stack]
    (when-let [v (peek s)]
      (swap! view-stack pop)
      [:> Button
       {:on-click
        #(vu/render-view v)}
       "Close"])))

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
        view
        `[:div {:class "view"}
          [:div {:class "main"}
           [:> ~Card {:variant "outlined"}
            [:> ~CardContent
             [:> ~Typography {:gutterBottom true :variant "h5" :component "div"}
              ~title][:br]
             ~@(render-attribute-specs
                rec-name scm
                (mapv
                 u/string-as-keyword
                 (:Fields instance))
                [(:QueryBy instance) (:QueryValue instance)]
                get-state-value change-handler)
             [:> ~Button
              {:on-click
               ~#(let [inst (transformer @inst-state)]
                   (vu/fire-upsert
                    rec-name inst
                    (partial upsert-callback rec-name table-view-id)))}
              "Create"]
             ~@(navigation-buttons rels rec-name)]
            ~(close-button)]
           [:div {:id ~table-view-id}]]]]
    (swap! view-stack conj view)
    (assoc instance :View view)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
