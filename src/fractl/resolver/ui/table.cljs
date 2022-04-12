(ns fractl.resolver.ui.table
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
            [fractl.resolver.ui.util :as vu]
            ["@material-ui/core"
             :refer [Card CardContent
                     Typography ButtonGroup Button
                     InputLabel Divider
                     TableContainer Table
                     TableRow TableHead
                     TableBody TableCell]]))

(defn upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        source-event (cn/make-instance
                      (u/string-as-keyword (:Source instance))
                      {})
        [_ n] (li/split-path rec-name)
        id (str n "-table-view")
        fields (mapv u/string-as-keyword (:Fields instance))
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
    (assoc instance :View table-view)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
