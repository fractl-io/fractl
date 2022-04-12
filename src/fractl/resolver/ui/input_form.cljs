(ns fractl.resolver.ui.input-form
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            [fractl.resolver.ui.util :as vu]
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
       (println (str "search error - " r))))
   search-event-instance))

(defn- render-attribute-specs [rec-name schema fields
                               get-state-value change-handler]
  (let [fields (or fields (cn/attribute-names schema))]
    (interpose
     [:> TableContainer
      [:> Table
       [:> TableBody
        [:> Divider]]]]
     (mapv
      (fn [field]
        (let [has-field-spec (map? field)
              field-name (if has-field-spec (first (keys field)) field)
              field-spec (when has-field-spec (field-name field))
              search-event (:search-event field-spec)
              n (name field-name)
              id n
              attr-scm (cn/find-attribute-schema (field schema))
              default-value (str
                             (when-let [d (:default attr-scm)]
                               (if (fn? d) (d) d)))
              k field-name
              h (partial change-handler k)]
          [:> TableRow
           [:> TableCell
            [:> TextField
             {:id id
              :label n
              :variant "standard"
              :on-change h}]]
           [:> TableCell
            (when search-event
              (make-find-button
               id get-state-value k
               search-event))]]))
      fields))))

(defn- upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        [_ r] (li/split-path rec-name)
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
                rec-name scm (mapv u/string-as-keyword (:Fields instance))
                get-state-value change-handler)
             [:> ~Button
              {:on-click
               ~#(let [inst (transformer @inst-state)]
                   (vu/fire-upsert rec-name inst)
                   (rdom/render
                    [(fn [] nil)]
                    (-> js/document
                        (.getElementById table-view-id))))}
              "Create"]]]
           [:div {:id ~table-view-id}]]]]
    (assoc instance :View view)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
