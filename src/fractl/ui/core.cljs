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
            [fractl.resolver.registry :as rg]
            [fractl.meta :as mt]
            [fractl.util.logger :as log]
            [fractl.ui.model]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            [fractl.ui.config :as cfg]
            [fractl.resolver.ui.table :as vt]
            [fractl.resolver.ui.instance :as vi]
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

(defn make-home-link [rec-name n link-text]
  (let [s (str vu/link-prefix "/" link-text)]
    [rec-name
     [:a {:href s}
      (str (if (cfg/views-authorize? rec-name)
             "Logout"
             (name n))
           " | ")]]))

(defn- app-routes [config]
  (secretary/set-config! :prefix vu/link-prefix)
  (vu/clear-home-links!)
  (loop [ens (cn/displayable-record-names (cfg/component config))]
    (if-let [en (first ens)]
      (let [[_ n] (li/split-path en)
            s (s/lower-case (vu/display-name n))
            schema (cn/entity-schema en)
            meta (cn/fetch-meta en)]
        (defroute (vu/make-dashboard-route s) []
          (v/render-main-view
           (v/make-dashboard-view en)))
        (defroute (vu/make-list-view-route s) []
          (v/render-main-view
           (v/make-list-view en)))
        (doseq [uq (cn/unique-attributes schema)]
          (defroute (vu/make-instance-view-route s uq) {:as params}
            (v/render-main-view
             (v/make-instance-view [en uq (:s params)]))))
        (doseq [cnt (mt/contains meta)]
          (let [[_ cn :as sn] (li/split-path cnt)]
            (defroute (vu/make-contains-route s cn) {:as params}
              (v/render-main-view
               (v/make-list-view
                (vu/make-multi-arg-query-event-spec
                 sn [n (:id1 params) cn (:id2 params)]))))))
        (when (cfg/views-authorize? en)
          (vu/set-authorization-required! en))
        (when-let [cns (seq (mt/contains meta))]
          (vu/ignore-in-home-links! cns))
        (vu/attach-home-link! (make-home-link en n s))
        (recur (rest ens)))
      (defroute "/" []
        (v/render-home-view
         "Home Page" (cfg/dashboard config)))))
  (hook-browser-navigation!))

(defn- process-post-init-result [r]
  (let [fr (if (cn/event-instance? r)
             (e/ok-result (e/eval-all-dataflows r))
             r)]
    (log/info (str "UI post-init - " fr))))

(defn init-view
  ([config post-init]
   (gs/merge-app-config! config)
   (when-let [h (:remote-api-host config)]
     (vu/set-remote-api-host! h))
   (rg/override-resolver
    [:Fractl.UI/InputForm]
   (vif/make :input-form nil))
   (rg/override-resolver
    [:Fractl.UI/InstanceForm]
    (vi/make :instance nil))
   (rg/override-resolver
    [:Fractl.UI/Table :Fractl.UI/Dashboard]
    (vt/make :table nil))
   (when post-init
     (process-post-init-result (post-init)))
   (app-routes config))
  ([post-init]
   (init-view nil post-init)))
