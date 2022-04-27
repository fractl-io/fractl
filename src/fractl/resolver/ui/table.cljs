(ns fractl.resolver.ui.table
  (:require [reagent.core :as r]
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

(defn- render-instance [fields link-prefix inst]
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
             [:> Link {:href (str link-prefix (:Id inst))} s])])))
      (vec result))))

(defn- render-rows [rows fields elem-id]
  (when (seq rows)
    (let [headers (mapv (fn [f] [:> TableCell (name f)]) fields)
          n (name (second (li/split-path (cn/instance-name (first rows)))))
          link (str "#/" n "?Id=")
          r (partial render-instance fields link)
          table-rows
          (mapv
           (fn [inst]
             `[:> ~TableRow
               ~@(r inst)])
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
           (.getElementById elem-id))))))

(def ^:private interval-handle (atom nil))

(defn- set-interval! [callback ms]
  (when-let [h @interval-handle]
    (js/clearInterval h))
  (reset!
   interval-handle
   (js/setInterval callback ms)))

;; TODO: table-view should be created in input-form,
;; so that auto-update is available always,
;; or create table on input-form display itself - do not
;; wait for Create button event.
(defn upsert-ui [instance]
  (let [rec-name (u/string-as-keyword (:Record instance))
        source-event (cn/make-instance
                      (u/string-as-keyword (:Source instance))
                      {})
        [_ n] (li/split-path rec-name)
        id (str n "-table-view")
        fields (mapv u/string-as-keyword (:Fields instance))
        table-view [:div {:id id}]
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
    (set-interval! data-refresh! data-refresh-ms)
    (assoc instance :View table-view)))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
