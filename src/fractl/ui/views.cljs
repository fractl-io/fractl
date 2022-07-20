(ns fractl.ui.views
  (:require [clojure.string :as s]
            [reagent.dom :as rdom]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.meta :as mt]
            [fractl.lang.internal :as li]
            [fractl.ui.config :as cfg]
            [fractl.ui.util :as vu]
            [fractl.ui.context :as ctx]
            ["@material-ui/core"
             :refer [Button]]))

(declare make-input-view)

(defn- make-view [tag target-info]
  (let [auth-rec-name (vu/authorization-record-name)
        target-info (or auth-rec-name
                        (if (string? target-info)
                          (keyword target-info)
                          target-info))
        is-inst (cn/an-instance? target-info)
        is-raw-spec (and (not is-inst) (map? target-info))
        is-query-spec (and (not is-inst)
                           (not is-raw-spec)
                           (seqable? target-info))
        [rec-name final-entity-spec]
        (cond
          is-inst [(cn/instance-type target-info)
                   {:instance target-info}]
          is-query-spec [(first target-info)
                         {:query-info target-info}]
          :else [(if is-raw-spec
                   (:record target-info)
                   target-info)
                 (when is-raw-spec target-info)])]
    (if (and (cfg/views-authorize? rec-name) (not auth-rec-name))
      (do (vu/set-authorization-record-name! rec-name)
          (make-input-view target-info))
      (let [input-form-event
            (vu/make-render-event rec-name final-entity-spec tag)
            r (vu/eval-event nil true input-form-event)
            v (first (vu/eval-result r))]
        (or (:View v)
            (do (println (str "input form generation failed. " r))
                [:div "failed to generate view for " [rec-name tag]]))))))

(def make-instance-view (partial make-view :instance))
(def make-list-view (partial make-view :list))
(def make-input-view (partial make-view :input))
(def make-dashboard-view (partial make-view :dashboard))

(def main-view-id "main-view")

(defn render-view
  ([view-spec elem-id]
   (rdom/render
    [(fn [] view-spec)]
    (-> js/document
        (.getElementById elem-id)))
   (vu/run-post-render-events!))
  ([view-spec]
   (render-view view-spec main-view-id)))

(defn render-main-view [view-spec]
  (vu/reset-page-state!)
  (render-view view-spec))


(defn- make-home-view [title dashboard-entity]
  (if-let [auth-rec-name (vu/authorization-record-name)]
    [:div {:id main-view-id} (make-input-view auth-rec-name)]
    (let [dv (make-dashboard-view dashboard-entity)]
      `[:div {:style {:display "flex"}}
        [:div {:id "sidebar" :style {:background "#111827 
        " :padding-left "20px" :padding-top "50px" :padding-right "50px" :height "100vh" :flex "1"}}
         [:div {:style {:background "rgba(255, 255, 255, 0.04)" :padding-left "24px" :padding-top "20px" :padding-bottom "20px" :border-radius "8px" :color "white"}}
           "Fractl Inc" ]
         [:a {:href "#"} [:h3
                          {:style {:padding-left "20px"
                            :margin-top "10px"
                                  :display "white"
                                   :color "lightgrey"
                                   :text-decoration "none"}} ~title]]
         [:div ~@(vu/fetch-home-links)]]
         [:div {:id ~main-view-id :style {:flex "8"}}
         [:div ~dv]]
        
         ])))

(defn render-home-view
  ([spec]
   (vu/reset-page-state!)
   (render-view spec "app"))
  ([title dashboard-entity]
   (render-home-view (make-home-view title dashboard-entity)))
  ([] (render-home-view "Home" (cfg/dashboard))))

(defn- query-and-make-dashboard-view [instance rec-name
                                      ref-rec-name
                                      [ref-attr refs]]
  (let [qevent (vu/make-query-event ref-rec-name ref-attr (get-in instance refs))
        target-id (str "list-" (name ref-rec-name))
        v (make-dashboard-view {:record ref-rec-name :source qevent})]
    `[:div {:id ~target-id} ~v]))

(defn make-list-refs-view
  ([rec-name instance meta]
   (when-let [lrs (mt/contains meta)]
     (mapv
      #(if (cn/event? %)
         [:> Button
          {:on-click (fn [] (render-view (make-input-view %)))}
          (vu/display-name %)]
         (when-let [scms (seq (cn/ref-attribute-schemas (cn/fetch-schema %)))]
           (when-let [r (vu/ref-to-record rec-name scms)]
             (query-and-make-dashboard-view instance rec-name % r))))
      lrs)))
  ([instance]
   (let [n (cn/instance-type instance)]
     (make-list-refs-view n instance (cn/fetch-meta n)))))
