(ns fractl.ui.core
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [secretary.core :as secretary]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [fractl.global-state :as gs]
            [fractl.evaluator :as e]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.meta :as mt]
            [fractl.util.logger :as log]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            [fractl.ui.config :as cfg]
            ["@material-ui/core" :refer [Link]])
  (:import goog.history.Html5History)
  (:require-macros [secretary.core :refer [defroute]]))

(defn hook-browser-navigation! []
  (doto (Html5History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn make-home-link [rec-name n]
  (let [is-auth-rec (cfg/views-authorize? rec-name)]
    [rec-name
     [:div {:style {:display "flex" :align-items "center" :margin-top "10px" :margin-bottom "10px"}}
      [:> Link
       {:component "button"
        :variant "body2"
        :style {:color "white" :padding-left "20px"}
        :on-click #(do (when is-auth-rec
                         (vu/clear-authorization!))
                       (if (cn/event? rec-name)
                         (v/render-view
                          (v/make-input-view rec-name))
                         (v/render-main-view
                          (v/make-dashboard-view rec-name))))}

       (str (if is-auth-rec
              "Logout"
              (name n)))]]]))

(defn- app-routes [config]
  (secretary/set-config! :prefix vu/link-prefix)
  (vu/clear-home-links!)
  [:div {:style {:color "red"}}
   (loop [ens (cn/displayable-record-names (cfg/component config))]
     (if-let [en (first ens)]
       (let [[_ n] (li/split-path en)
             schema (cn/entity-schema en)
             meta (cn/fetch-meta en)]
         (defroute (vu/make-dashboard-route n) []
           (v/render-main-view
            (v/make-dashboard-view en)))
         (defroute (vu/make-list-view-route n) []
           (v/render-main-view
            (v/make-list-view en)))
         (doseq [uq (cn/unique-attributes schema)]
           (defroute (vu/make-instance-view-route n uq) {:as params}
             (v/render-main-view
              (v/make-instance-view [en uq (:s params)]))))
         (doseq [cnt (mt/contains meta)]
           (let [[_ cn :as sn] (li/split-path cnt)]
             (defroute (vu/make-contains-route n cn) {:as params}
               (v/render-main-view
                (v/make-list-view
                 (vu/make-multi-arg-query-event-spec
                  sn [n (:id1 params) cn (:id2 params)]))))))
         (when (cfg/views-authorize? en)
           (vu/set-authorization-record-name! en))
         (when-let [cns (seq (mt/contains meta))]
           (vu/ignore-in-home-links! cns))
         (vu/attach-home-link! (make-home-link en n))
         (recur (rest ens)))
       (defroute "/" []
         (v/render-home-view
          "Home Page" (cfg/dashboard config)))))]

  (hook-browser-navigation!))

(defn- process-post-init-result [r]
  (let [fr (if (cn/event-instance? r)
             (e/ok-result (e/eval-all-dataflows r))
             r)]
    (log/info (str "UI post-init - " fr))))

(defn- post-init [post-init-event]
  ;; Do fractl post-init stuff here,
  ;; e.g register custom UI resolvers.
  (cn/make-instance
   {post-init-event {}}))

(defn init-view
  ([config post-init-event]
   (gs/merge-app-config! config)
   (when-let [h (:remote-api-host config)]
     (vu/set-remote-api-host! h))
   (when post-init-event
     (process-post-init-result
      (post-init post-init-event)))
   (app-routes config))
  ([post-init]
   (init-view nil post-init)))
