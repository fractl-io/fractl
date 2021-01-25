(ns fractl.ui.core
  (:require [clojure.walk :as w]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.lang :as f]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rr]))

(defn core-ui-upsert [inst]
  (rgdom/render
   [(:View inst)]
   (.getElementById js/document (:DOM_Target inst))))

(def click-count (rg/atom 0))

(defn atom-input [v]
  [:input {:type "text"
           :value @v
           :on-change #(reset! v (-> % .-target .-value))}])

(def text-val (rg/atom "hi"))

(defn input-component []
  [:div
   [:p "The value is now: " @text-val]
   [:p "Change it here: " [atom-input text-val]]])

(defn counter-component []
  [:div
   "The atom " [:code "click-count"] " has value: "
   @click-count ". "
   [:input {:type "button" :value "Click me!"
            :on-click #(swap! click-count inc)}]])

(f/component :BasicUI)

(f/entity {:BasicUI/Greeting
           {:View :Kernel/Any
            :DOM_Target :Kernel/String}})

(f/entity {:BasicUI/Counter
           {:ClickCount {:type :Kernel/Cell
                         :default (f/cell 0)}
            :View {:expr '(identity
                           [:div
                            "The atom " [:code "click-count"] " has value: "
                            (:deref :ClickCount) ". "
                            [:input {:type "button" :value "Click me!"
                                     :on-click #(:swap! :ClickCount :inc)}]])}}})

(f/event {:BasicUI/ShowGreeting
          {:Message :Kernel/String}})

(f/dataflow :BasicUI/ShowGreeting
            {:BasicUI/Counter {}}
            {:BasicUI/Greeting
             {:View [:q# [:div [:p [:uq# :BasicUI/ShowGreeting.Message]]
                          ;;[:div [:uq# :BasicUI/Counter]]
                          [:div [counter-component]]
                          [:div [input-component]]]]
              :DOM_Target "app"}})

(defn- embedded-views-as-fns [view-spec]
  (w/prewalk (fn [x]
               (cond
                 (cn/entity-instance? x)
                 (:View x)
                 (and (seqable? x) (= :deref (first x)))
                 `(deref ~(second x))
                 :else x))
             view-spec))

(defn- basic-ui-upsert [inst]
  (core-ui-upsert
   (assoc inst :View (fn [] (embedded-views-as-fns (:View inst))))))

(defn make-ui-resolver [resolver-name upsert]
  (r/make-resolver resolver-name {:upsert upsert}))

(rr/override-resolver
 :BasicUI/Greeting
 (make-ui-resolver :BasicUI-Resolver basic-ui-upsert))

(let [evt (cn/make-instance {:BasicUI/ShowGreeting
                             {:Message "hello, world"}})
      result (e/eval-all-dataflows evt)]
  (println result))
