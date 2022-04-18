(ns fractl.ui.core
  (:require [clojure.string :as s]
            [reagent.core :as r]
            [secretary.core :as secretary]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.resolver.registry :as rg]
            [fractl.resolver.ui.model]
            [fractl.ui.util :as vu]
            [fractl.resolver.ui.table :as vt]
            [fractl.resolver.ui.input-form :as vif])
  (:import goog.history.Html5History)
  (:require-macros [secretary.core :refer [defroute]]))

(def app-state (r/atom {}))

(defn hook-browser-navigation! []
  (doto (Html5History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defmulti current-page #(@app-state :page))

(defn- app-routes [component]
  (secretary/set-config! :prefix "#")

  (defroute "/" []
    (swap! app-state assoc :page :home))

  (loop [ens (cn/entity-names component)
         home-links []]
    (if-let [en (first ens)]
      (let [[_ n] (li/split-path en)
            s (s/lower-case (name n))]
        (defroute (str "/" s) []
          (swap! app-state assoc :page en))
        (defmethod current-page en []
          [(fn [] (vu/generate-view en))])
        (recur
         (rest ens)
         (conj
          home-links
          [:a {:href (str "#/" s)} (str (name n) " | ")])))
      (defmethod current-page :default []
        [(fn []
           `[:div [:h1 "Home Page"]
             ~@home-links])])))
  (hook-browser-navigation!))

(defn init-view [config]
  (when-let [h (:remote-api-host config)]
    (vu/set-remote-api-host! h))
  (rg/override-resolver
   [:Fractl.UI/InputForm]
   (vif/make :input-form nil))
  (rg/override-resolver
   [:Fractl.UI/Table]
   (vt/make :table nil))  
  (app-routes (get-in config [:view :component]))
  current-page)
