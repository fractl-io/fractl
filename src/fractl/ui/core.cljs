(ns fractl.ui.core
  (:require [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.lang :as f]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rr]))

(defn upsert [inst]
  (rgdom/render
   [(:View inst)]
   (.getElementById js/document (:DOM_Target inst))))

(defn make-ui-resolver [resolver-name]
  (r/make-resolver resolver-name {:upsert upsert}))

(f/component :FractlUI)
(f/entity {:FractlUI/SimpleComponent
           {:View :Kernel/Any
            :DOM_Target :Kernel/String}})

(def ui-resolver (make-ui-resolver :UI-Resolver))
(rr/override-resolver :FractlUI/SimpleComponent ui-resolver)

(defn install-component
  ([target view]
   (let [c (cn/make-instance {:FractlUI/SimpleComponent
                              {:View view
                               :DOM_Target target}})
         evt (cn/make-instance {:FractlUI/Upsert_SimpleComponent
                                {:Instance c}})]
     (ffirst (:result (first (e/eval-all-dataflows evt))))))
  ([view]
   (install-component "app" view)))

(defn greet []
  (install-component (fn [] [:h2 "Hello Fractl World!"])))

(def click-count (rg/atom 0))

(defn counter []
  (install-component
   (fn [] [:div
           "The atom " [:code "click-count"] " has value: "
           @click-count ". "
           [:input {:type "button" :value "Click me!"
                    :on-click #(swap! click-count inc)}]])))

(defn atom-input [val]
  [:input {:type "text"
           :value @val
           :on-change #(do (println @val %) (reset! val (-> % .-target .-value)))}])

(defn state-demo []
  (let [val (rg/atom "hi")]
    (install-component
     (fn [] [:div
             [:p "The value is now: " @val]
             [:p "Change it here: " [atom-input val]]]))))

;;(counter)
(state-demo)
