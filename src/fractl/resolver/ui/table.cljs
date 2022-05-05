(ns fractl.resolver.ui.table
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.global-state :as gs]
            [fractl.ui.util :as vu]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            ["@material-ui/core"
             :refer [Card CardContent Link
                     Typography ButtonGroup Button
                     InputLabel Divider
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(defn- render-instance [fields rec-name inst]
  (loop [fields fields, linked false, result []]
    (if-let [f (first fields)]
      (let [s (vu/decode-to-str (get inst f))]
        (recur
         (rest fields)
         true
         (conj
          result
          [:> TableCell
           (if linked
             s
             [:> Link
              {:component "button"
               :variant "body2"
               :on-click #(do (vu/reset-page-state!)
                              (vu/render-view
                               (vu/make-instance-view inst)))}
              s])])))
      (vec result))))

(defn- render-rows [rows fields elem-id]
  (when (seq rows)
    (let [headers (mapv (fn [f] [:> TableCell (name f)]) fields)
          rec-name (cn/instance-name (first rows))
          n (name (second (li/split-path rec-name)))
          r (partial render-instance fields rec-name)
          table-rows
          (mapv
           (fn [inst]
             `[:> ~TableRow
               ~@(r inst)])
           rows)]
      (vu/render-view
       `[:> ~TableContainer
         [:> ~Table
          [:> ~TableHead
           [:> ~TableRow
            ~@headers]]
          [:> ~TableBody
           ~@table-rows]]]
       elem-id))))

;; TODO: table-view should be created in input-form,
;; so that auto-update is available always,
;; or create table on input-form display itself - do not
;; wait for Create button event.
(defn upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        src (:Source instance)
        has-source-event (map? src)
        source-event (if has-source-event
                       src
                       (cn/make-instance
                        (u/string-as-keyword src)
                        {}))
        [_ n] (li/split-path rec-name)
        id (str n "-table-view")
        fields (mapv u/string-as-keyword (:Fields instance))
        table-view [:div [:div {:id id}]
                    (when (= :Dashboard (second (li/split-path (cn/instance-name instance))))
                      [:> Button
                       {:on-click #(vu/render-view
                                    (vu/make-input-view rec-name))}
                       (str "Create New " (name n))])]
        data-refresh!
        #(vu/eval-event
          (fn [result]
            (if-let [rows (vu/eval-result result)]
              (render-rows rows fields id)
              (println (str "failed to list " rec-name " - " result))))
          source-event)
        data-refresh-ms
        (or (get-in
             (gs/get-app-config)
             [:ui :data-refresh-ms rec-name])
            5000)]
    (data-refresh!)
    (vu/set-interval! data-refresh! data-refresh-ms)
    (assoc instance :View table-view)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
