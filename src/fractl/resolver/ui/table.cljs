(ns fractl.resolver.ui.table
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.global-state :as gs]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            [fractl.ui.context :as ctx]
            [fractl.ui.meta :as mt]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as rc]
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
                              (ctx/attach-to-context! inst)
                              (v/render-view
                               (v/make-instance-view inst)))}
              s])])))
      (vec (conj result (delete-instance-button rec-name (:Id inst)))))))

(defn- make-rows-view [rows fields]
  (if (seq rows)
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
      `[:> ~TableContainer
        [:> ~Table
         [:> ~TableHead
          [:> ~TableRow
           ~@headers]]
         [:> ~TableBody
          ~@table-rows]]])
    [:div "no data"]))

(defn- render-rows [rows fields elem-id]
  (v/render-view
   (make-rows-view rows fields)
   elem-id))

;; TODO: table-view should be created in input-form,
;; so that auto-update is available always,
;; or create table on input-form display itself - do not
;; wait for Create button event.
(defn upsert-ui [instance]
  (assoc
   instance :View
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
                         (when (= :Dashboard (second (li/split-path (cn/instance-name instance))))
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
         table-view)))))

(defn make [resolver-name _]
  (rc/make-resolver
   resolver-name
   {:upsert {:handler upsert-ui}}))
