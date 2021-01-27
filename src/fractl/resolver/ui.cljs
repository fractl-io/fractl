(ns fractl.resolver.ui
  (:require [clojure.walk :as w]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.lang.internal :as li]))

(defn- upsert [[inst 
  (let [target (:DOM_Target inst)
        view-fn (parse-view (:View inst))]
    (rgdom/render
     [view-fn]
     (.getElementById js/document target))))
