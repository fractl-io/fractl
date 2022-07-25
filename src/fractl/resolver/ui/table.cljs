(ns fractl.resolver.ui.table
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.global-state :as gs]
            [fractl.meta :as mt]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            [fractl.ui.context :as ctx]
            [fractl.ui.style :as style]
            [fractl.ui.config :as cfg]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            ["@material-ui/core"
             :refer [Link Button Paper
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(defn- delete-instance-button [rec-name id]
  [:> TableCell
   [:> Button
    {:component "button"
     :variant "contained"
     :style {:background "#800000" :color "white"}
     :on-click
     #(vu/fire-delete-instance
       rec-name id
       (mt/delete-event (cn/fetch-meta rec-name)))}
    "Delete"]])

(defn- render-instance [cell-style fields rec-name inst]
  (let [n (cn/instance-type inst)
        schema (cn/fetch-schema n)]
    (loop [fields fields, linked false, result []]
      (if-let [f (first fields)]
        (let [disp-v (vu/attr-val-display inst schema f)]
          (recur
           (rest fields)
           true
           (conj
            result
            [:> TableCell cell-style
             (if linked
               disp-v
               [:> Link
                {:component "button"
                 :variant "body2"
                 :on-click #(do (vu/reset-page-state!)
                                (ctx/attach-to-context! inst)
                                (v/render-view
                                 (v/make-instance-view inst)))}
                disp-v])])))
        (vec (conj result (delete-instance-button rec-name (cn/id-attr inst))))))))

(declare make-rows-view)

(defn- page-nav-button [pages all-rows fields elem-id label offset]
  [:> Button
   {:on-click
    (fn []
      (reset! pages [offset all-rows])
      (v/render-view
       (make-rows-view pages fields elem-id)
       elem-id))}
   label])

(defn- make-rows-view [pages fields elem-id]
  (let [ps @pages
        all-rows (second ps)
        offset (first ps)
        rows (if offset (nth all-rows offset) all-rows)]
    (if (seq rows)
      (let [rec-name (cn/instance-type (first rows))
            styles (cfg/views-styles rec-name)
            table-head-cell-style (style/table-head-cell styles)
            headers (mapv (fn [f] [:> TableCell {:style {:text-transform "uppercase" :text-align (when (= f "Delete") "center")}}  (name f)]) (conj fields "Delete"))
            n (name (second (li/split-path rec-name)))
            r (partial render-instance (style/table-body-cell styles) fields rec-name)
            table-body-row-style (style/table-body-row styles)
            table-rows
            (mapv
             (fn [inst]
               `[:> ~TableRow ~table-body-row-style
                 ~@(r inst)])
             rows)
            n-all-rows (count all-rows)
            mkbtn (partial page-nav-button pages all-rows fields elem-id)
            next-btn (when (and offset (< offset (dec n-all-rows)))
                       (mkbtn "Next" (inc offset)))
            back-btn (when (and offset (> offset 0))
                       (mkbtn "Prev" (dec offset)))]
        `[:div {:style {:width "1200px" :overflow-x "auto" :height "750px" :text-align "center"}}
          [:> ~Paper  [:> ~TableContainer
                       [:> ~Table
                        [:> ~TableHead
                         [:> ~TableRow
                          ~@headers]]
                        [:> ~TableBody
                         ~@table-rows]]]]
          [:div ~back-btn ~next-btn]])
      [:div "no data"])))

(defn- paginate [rows rows-per-page]
  (atom
   (if rows-per-page
     [0 (partition rows-per-page rows)]
     [nil rows])))

(defn- render-rows [rows fields rows-per-page elem-id]
  (v/render-view
   (make-rows-view (paginate rows rows-per-page) fields elem-id)
   elem-id))

(defn- get-config [rec-name k]
  (get-in
   (gs/get-app-config)
   [:ui :dashboard rec-name k]))

(defn- make-view [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        [_ n] (li/split-path rec-name)
        id (str n "-table-view")
        cfg (partial get-config rec-name)
        fields (or
                 (cfg :order)
                (mapv u/string-as-keyword (:list (:Fields instance))))
        src (:Source instance)
        rows-per-page (cfg :rows-per-page)]
      (js/console.log (clj->js (:list (:Fields instance))))
    (if (vector? src)
      [:div (make-rows-view (paginate src rows-per-page) fields id)]
      (let [has-source-event (map? src)
            source-event (if has-source-event
                           src
                           (cn/make-instance
                            (u/string-as-keyword src)
                            {}))
            table-view [:div {:style {:text-align "center"}} [:div {:id id :style {:padding "30px 0px" :display "flex" :justify-content "center"}}]
                        (when (and (= :Dashboard (second (li/split-path (cn/instance-type instance))))
                                   (not= (cfg :create-new-button) :none))
                          [:> Button
                           {:style {:background "#24252a" :color "white"}
                            :on-click #(v/render-view
                                        (v/make-input-view rec-name))}
                           (str "Create New " (name n))])]
            data-refresh!
            #(vu/eval-event
              (fn [result]
                (if-let [rows (vu/eval-result result)]
                  (render-rows rows fields rows-per-page id)
                  (if (= :not-found (:status (first result)))
                    (v/render-view [:div (str (name rec-name) " - not found")] id)
                    (println (str "failed to list " rec-name " - " result)))))
              source-event)
            data-refresh-ms
            (when-not rows-per-page
              (or (cfg :data-refresh-ms) 5000))]
        (data-refresh!)
        (when data-refresh-ms
          (vu/set-interval! data-refresh! data-refresh-ms))
        table-view))))

(defn upsert-ui [instance]
  (vu/finalize-view (make-view instance) instance))

(defn make [resolver-name]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
