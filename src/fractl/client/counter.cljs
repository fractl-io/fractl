(ns fractl.client.counter
  (:require [fractl.component :as cn]
            [fractl.lang :refer [event entity dataflow]]
            [fractl.client.view]
            [fractl.evaluator :as e]
            [fractl.client.util :as u :refer-macros [defcomponent]]))

(defcomponent :UI_Counter
  (entity {:UI_Counter/Container
           {:DOM_View {:listof :Kernel/Any
                       :default []}}})
  
  (entity {:UI_Counter/Counter
           {:Count :Kernel/Int}})

  (event :UI_Counter/CreateCounter
         {:Name :Kernel/String})

  (dataflow :UI_Counter/CreateCounter
            {:UI_Counter/Counter {:Count 10} :as :C1}
            {:UI_Counter/Container {:DOM_View
                                    [:q# [:div "Hi" [:div [:uq# :C1.Count]]]]}
             :as :CV}
            {:Fractl.View/Root {:DOM_View :CV.DOM_View}}))

(defn run-test
  []
  (try
    (let [cevt (cn/make-instance :UI_Counter/CreateCounter {:Name "My Counter"})]
      (ffirst (u/fresult (e/eval-all-dataflows cevt))))
    (catch js/Error e (.log js/console e (.-stack e)))))
