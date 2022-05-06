(ns fractl.ui.core
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [secretary.core :as secretary]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [fractl.global-state :as gs]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.resolver.registry :as rg]
            [fractl.ui.model]
            [fractl.ui.meta :as mt]
            [fractl.ui.util :as vu]
            [fractl.ui.config :as cfg]
            [fractl.resolver.ui.table :as vt]
            [fractl.resolver.ui.input-form :as vif])
  (:import goog.history.Html5History)
  (:require-macros [secretary.core :refer [defroute]]))

(defn hook-browser-navigation! []
  (doto (Html5History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn make-home-link [meta rec-name link-text]
  (let [s (str "#/" link-text)]
    [:a {:href s}
     (str (if (mt/authorize? meta)
            "Logout"
            (name rec-name))
          " | ")]))

(defn- app-routes [config]
  (secretary/set-config! :prefix "#")
  (vu/clear-home-links!)

  (loop [ens (cn/displayable-record-names (cfg/component config))]
    (if-let [en (first ens)]
      (let [[_ n] (li/split-path en)
            s (s/lower-case (name n))
            schema (cn/entity-schema en)
            meta (cn/fetch-meta en)]
        (defroute (str "/" s) []
          (vu/render-app-view
           (vu/generate-dashboard-view en)))
        (defroute (str "/" s "/list") []
          (vu/render-app-view
           (vu/generate-list-view en)))
        (doseq [uq (cn/unique-attributes schema)]
          (defroute (str "/" s "/" (s/lower-case (name uq))) {:as params}
            (vu/render-app-view
             (vu/generate-instance-view [en uq (get-in params [:query-params :s])]))))
        (doseq [cnt (mt/contains meta)]
          (let [[_ cn :as sn] (li/split-path cnt)
                cs (s/lower-case (name cn))]
            (defroute (str "/" s "/:id1/" cs "/:id2") {:as params}
              (vu/render-app-view
               (vu/generate-list-view
                (vu/make-multi-arg-query-event-spec
                 sn [n (:id1 params) cn (:id2 params)]))))))
        (when (mt/authorize? meta)
          (vu/set-authorization-required! en))
        (vu/attach-home-link! (make-home-link meta n s))
        (recur (rest ens)))
      (defroute "/" []
        (vu/render-app-view
         (vu/make-home-view
          "Home Page" (cfg/dashboard config))))))
  (hook-browser-navigation!))

(defn init-view
  ([config post-init]
   (cfg/set-config! config)
   (when-let [h (:remote-api-host config)]
     (vu/set-remote-api-host! h))
   (rg/override-resolver
    [:Fractl.UI/InputForm :Fractl.UI/InstanceForm]
    (vif/make :input-form nil))
   (rg/override-resolver
    [:Fractl.UI/Table :Fractl.UI/Dashboard]
    (vt/make :table nil))
   (when post-init
     (post-init))
   (app-routes config))
  ([post-init] (init-view (gs/get-app-config) post-init))
  ([] (init-view nil)))
