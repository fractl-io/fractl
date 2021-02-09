(ns fractl.client.form
  (:require [fractl.component :as cn]
            [fractl.lang :refer [event entity dataflow]]
            [fractl.client.basic :as basic]
            [fractl.client.view :as view]
            [fractl.evaluator :as e]
            [fractl.store :as store]
            [fractl.client.util :as u :refer-macros [defcomponent]]))

(defcomponent :UI_Form
  (entity {:UI_Form/Counter
           {:Name :Kernel/String
            :Count :Kernel/String}})

  (event {:UI_Form/SetCount
          {:CId :Kernel/UUID
           :Count :Kernel/String}})

  (dataflow :UI_Form/SetCount
            {:UI_Form/Counter {:Id? :UI_Form/SetCount.CId
                               :Count :UI_Form/SetCount.Count}})

  (event {:UI_Form/GetCount
          {:CId :Kernel/UUID}})

  (dataflow :UI_Form/GetCount
            {:UI_Form/Counter {:Id? :UI_Form/SetCount.CId}})

  (event {:UI_Form/AddCount
          {:CId :Kernel/UUID}})

  (dataflow :UI_Form/AddCount
            {:UI_Form/Counter {:Id? :UI_Form/AddCount.CId
                               :Count "100"}})

  (event {:UI_Form/CreateCounter
          {:Name :Kernel/String}})

  (dataflow :UI_Form/CreateCounter
            {:UI_Form/Counter {:Name :UI_Form/CreateCounter.Name
                               :Count "10"} :as :C1}
            {:Fractl.Basic_UI/Component
             {:DOM_View
              [:q#
               [:div
                [:label [:uq# :C1.Name]]
                [:input {:type "text"
                         :value [:uq# :C1.Count]
                         :on-change [:uq# [:dispatch-on
                                           :Fractl.Basic_UI/DomEvent
                                           {:UI_Form/SetCount
                                            {:CId :C1.Id
                                             :Count :Fractl.Basic_UI/DomEvent.Value}}]]}]]]}
             :as :Label}
            {:Fractl.Basic_UI/Component
             {:DOM_View
              [:q#
               [:div
                [:input {:type "button"
                         :value "Add"
                         :on-click [:uq# [:dispatch-on
                                          :Fractl.Basic_UI/DomEvent
                                          {:UI_Form/AddCount
                                           {:CId :C1.Id}}]]}]]]}
             :as :AddButton}
            {:Fractl.Basic_UI/Component
             {:DOM_View
              [:q# [:div
                    [:div [:uq# :Label.DOM_View]]
                    [:div [:uq# :AddButton.DOM_View]]
                    [:div [:uq# :C1.Count]]]]}
             :as :CV}
            {:Fractl.View/Root {:DOM_View :CV.DOM_View
                                :DOM_Target "app"}}))

(def resolvers [view/view-resolver basic/comp-resolver])
(defn run-test
  []
  (try
    (println resolvers)
    (let [cevt (cn/make-instance :UI_Form/CreateCounter {:Name "My Counter"})]
      (ffirst (u/fresult (u/eval-dataflows-for-event cevt))))
    (catch js/Error e (.log js/console e (.-stack e)))))
