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
            [reagent-mui.x.data-grid :refer [data-grid]]
            [reagent-mui.util :refer [wrap-clj-function]]
            ["@material-ui/core"
             :refer [Link Button
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(defn- delete-instance-button [rec-name id]
  [:> TableCell
   [:> Link
    {:component "button"
     :variant "body2"
     :on-click
     #(vu/fire-delete-instance
       rec-name id
       (mt/delete-event (cn/fetch-meta rec-name)))}
    "Delete"]])

(defn- render-instance [cell-style fields rec-name inst]
  (loop [fields fields, linked false, result []]
    (if-let [f (first fields)]
      (let [s (vu/decode-to-str (get inst f))]
        (recur
         (rest fields)
         true
         (conj
          result
          [:> TableCell cell-style
           (if linked
             s
             [:> Link
              {:component "button"
               :variant "body2"
               :on-click #(do (vu/reset-page-state!)
                              (ctx/attach-to-context! inst)
                              (v/render-view
                               (v/make-instance-view inst)))}
              s])])))
      (vec (conj result (delete-instance-button rec-name (:Id inst)))))))

(defn- make-table-header [field]
  (let [n (name field)]
    {:field n
     :headerName n
     :width 150}))

;; example data-grid data, remove after testing
(def sample-columns
  [{:field      :id
    :headerName "ID"
    :width      90}
   {:field      :first-name
    :headerName "First name"
    :width      150
    :editable   true}
   {:field      :last-name
    :headerName "Last name"
    :width      150
    :editable   true}
   {:field      :age
    :headerName "Age"
    :type       :number
    :width      110
    :editable   true}
   {:field       :full-name
    :headerName  "Full name"
    :description "This column has a value getter and is not sortable."
    :sortable    false
    :width       160
    :valueGetter (wrap-clj-function
                  (fn [params]
                    (str (get-in params [:row :first-name] "") " " (get-in params [:row :last-name] ""))))}])

(def sample-rows [{:id 1 :last-name "Snow" :first-name "Jon" :age 35}
                  {:id 2 :last-name "Lannister" :first-name "Cersei" :age 42}
                  {:id 3 :last-name "Lannister" :first-name "Jaime" :age 45}
                  {:id 4 :last-name "Stark" :first-name "Arya" :age 16}
                  {:id 5 :last-name "Targaryen" :first-name "Daenerys" :age nil}
                  {:id 6 :last-name "Melisandre" :first-name nil :age 150}
                  {:id 7 :last-name "Clifford" :first-name "Ferrara" :age 44}
                  {:id 8 :last-name "Frances" :first-name "Rossini" :age 36}
                  {:id 9 :last-name "Roxie" :first-name "Harvey" :age 65}])

(defn- make-rows-view [rows fields]
  (if (seq rows)
    (let [rec-name (cn/instance-type (first rows))
          styles (cfg/views-styles rec-name)
          table-head-cell-style (style/table-head-cell styles)
          headers; (mapv (fn [f] [:> TableCell table-head-cell-style (name f)]) fields)
          (mapv #(make-table-header %) fields)
          n (name (second (li/split-path rec-name)))
          r (partial render-instance (style/table-body-cell styles) fields rec-name)
          table-body-row-style (style/table-body-row styles)
          table-rows
          (mapv #(select-keys % fields) rows)
          #_(mapv
             (fn [inst]
               `[:> ~TableRow ~table-body-row-style
                 ~@(r inst)])
             rows)]
      [:div {:style {:height 400 :width "100%"}}
       [:> data-grid
        {:rows sample-rows ;table-rows
         :columns sample-columns ;headers
         :page-size 5
         :rows-per-page-options [5]}]]
      #_`[:> ~TableContainer
          [:> ~Table ~(style/table styles)
           [:> ~TableHead ~(style/table-head styles)
            [:> ~TableRow ~(style/table-head-row styles)
             ~@headers]]
           [:> ~TableBody ~(style/table-body styles)
            ~@table-rows]]])
    [:div "no data"]))

(defn- render-rows [rows fields elem-id]
  (v/render-view
   (make-rows-view rows fields)
   elem-id))

(defn- make-view [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        [_ n] (li/split-path rec-name)
        id (str n "-table-view")
        fields (mapv u/string-as-keyword (:Fields instance))
        src (:Source instance)]
    (if (vector? src)
      [:div (make-rows-view src fields)]
      (let [has-source-event (map? src)
            source-event (if has-source-event
                           src
                           (cn/make-instance
                            (u/string-as-keyword src)
                            {}))
            table-view [:div [:div {:id id}]
                        (when (= :Dashboard (second (li/split-path (cn/instance-type instance))))
                          [:> Button
                           {:on-click #(v/render-view
                                        (v/make-input-view rec-name))}
                           (str "Create New " (name n))])]
            data-refresh!
            #(vu/eval-event
              (fn [result]
                (if-let [rows (vu/eval-result result)]
                  (render-rows rows fields id)
                  (if (= :not-found (:status (first result)))
                    (v/render-view [:div (str (name rec-name) " - not found")] id)
                    (println (str "failed to list " rec-name " - " result)))))
              source-event)
            data-refresh-ms
            (or (get-in
                 (gs/get-app-config)
                 [:ui :data-refresh-ms rec-name])
                5000)]
        (data-refresh!)
        (vu/set-interval! data-refresh! data-refresh-ms)
        table-view))))

(defn upsert-ui [instance]
  (vu/finalize-view (make-view instance) instance))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
