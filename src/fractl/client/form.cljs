(ns fractl.client.form
  (:require [fractl.component :as cn]
            [fractl.lang :refer [event entity dataflow]]
            [fractl.client.basic]
            [fractl.client.view]
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

  (event {:UI_Form/ResetCount
          {:CId :Kernel/UUID}})

  (dataflow :UI_Form/ResetCount
            {:UI_Form/Counter {:Id? :UI_Form/ResetCount.CId
                               :Count "10"}})

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
                         :on-change [:uq# [:eval-on
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
                         :value "Reset"
                         :on-click [:uq# [:eval-on
                                          :Fractl.Basic_UI/DomEvent
                                          {:UI_Form/ResetCount
                                           {:CId :C1.Id}}]]}]]]}
             :as :ResetButton}
            {:Fractl.Basic_UI/Component
             {:DOM_View
              [:q# [:div
                    [:div [:uq# :Label.DOM_View]]
                    [:div "Count is: " [:uq# :C1.Count]]
                    [:div [:uq# :ResetButton.DOM_View]]]]}
             :as :CV}
            {:Fractl.View/Root {:DOM_View :CV.DOM_View
                                :DOM_Target "app"}}))

(defn run-test
  []
  (try
    (let [cevt (cn/make-instance :UI_Form/CreateCounter {:Name "My Counter"})]
      (ffirst (u/fresult (u/eval-dataflows-for-event cevt))))
    (catch js/Error e (.log js/console e (.-stack e)))))
