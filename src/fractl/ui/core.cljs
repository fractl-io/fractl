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
            [fractl.ui.util :as vu]
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

(defn- app-routes [component]
  (secretary/set-config! :prefix "#")

  (loop [ens (cn/displayable-record-names component)
         home-links []]
    (if-let [en (first ens)]
      (let [[_ n] (li/split-path en)
            s (s/lower-case (name n))
            schema (cn/entity-schema en)]
        (defroute (str "/" s) []
          (vu/render-app-view
           (vu/generate-view en)))
        (defroute (str "/" s "/list") []
          (vu/render-app-view
           (vu/generate-view en :list)))
        (doseq [uq (cn/unique-attributes schema)]
          (defroute (str "/" s "/" (name uq)) {:as params}
            (vu/render-app-view
             (vu/generate-view [en uq (get-in params [:query-params :s])] :display))))
        (recur
         (rest ens)
         (conj
          home-links
          [:a {:href (str "#/" s)} (str (name n) " | ")])))
      (defroute "/" []
        (vu/render-app-view
         `[:div [:h1 "Home Page"]
           ~@home-links]))))
  (hook-browser-navigation!))

(defn init-view
  ([config]
   (when-let [h (:remote-api-host config)]
     (vu/set-remote-api-host! h))
   (rg/override-resolver
    [:Fractl.UI/InputForm]
    (vif/make :input-form nil))
   (rg/override-resolver
    [:Fractl.UI/Table]
    (vt/make :table nil))
   (app-routes (get-in config [:view :component])))
  ([] (init-view (gs/get-app-config))))
