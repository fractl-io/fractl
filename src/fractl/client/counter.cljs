(ns fractl.client.counter
  (:require [fractl.component :as cn]
            [fractl.lang :refer [event entity dataflow]]
            [fractl.client.basic]
            [fractl.client.view]
            [fractl.evaluator :as e]
            [fractl.client.util :as u :refer-macros [defcomponent]]))

(defcomponent :UI_Counter
  (entity {:UI_Counter/Counter
           {:Count :Kernel/Int}})

  (event :UI_Counter/CreateCounter
         {:Name :Kernel/String})

  (dataflow :UI_Counter/CreateCounter
            {:UI_Counter/Counter {:Count 10} :as :C}
            {:Fractl.Basic_UI/Component {:DOM_View
                                         [:q# [:div "Count is " [:div [:uq# :C.Count]]]]}
             :as :CComp}
            {:Fractl.View/Root {:DOM_View :CComp.DOM_View
                                :DOM_Target "app"}}))

(defn run-test
  []
  (try
    (let [cevt (cn/make-instance :UI_Counter/CreateCounter {:Name "My Counter"})]
      (ffirst (u/fresult (e/eval-all-dataflows cevt))))
    (catch js/Error e (.log js/console e (.-stack e)))))
