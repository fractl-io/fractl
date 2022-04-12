(ns fractl.resolver.ui.table
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.resolver.ui.util :as vu]
            ["@material-ui/core"
             :refer [Card CardContent
                     Typography ButtonGroup Button
                     InputLabel Divider
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(defn- fetch-table-spec [rec-name meta]
  (if-let [spec (get-in meta [:views :list :Fractl.UI/Table])]
    spec
    (u/throw-ex (str "no :meta for table view - " rec-name))))

(defn realize-ui [rec-name]
  (let [meta (cn/fetch-meta rec-name)
        table-spec (fetch-table-spec rec-name meta)
        source-event (cn/make-instance
                      (:Source table-spec)
                      {})
        [_ n] (li/split-path rec-name)
        id (str n "-table-view")
        fields (:Fields table-spec)
        table-view [:div {:id id}]]        
    (vu/eval-event
     (fn [result]
       (if-let [rows (vu/eval-result result)]
         (let [headers (mapv (fn [f] [:> TableCell (name f)]) fields)
               table-rows (mapv (fn [inst]
                                  `[:> ~TableRow
                                    ~@(mapv
                                       (fn [f]
                                         [:> TableCell (str (get inst f))])
                                       fields)])
                                rows)]
           (rdom/render
            [(fn []
               `[:> ~TableContainer
                 [:> ~Table
                  [:> ~TableHead
                   [:> ~TableRow
                    ~@headers]]
                  [:> ~TableBody
                   ~@table-rows]]])]
            (-> js/document
                (.getElementById id))))
         (println (str "failed to list " rec-name " - " result))))
     source-event)
    table-view))
