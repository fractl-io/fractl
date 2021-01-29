(ns fractl.ui.core
  (:require [clojure.walk :as w]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.lang :as f]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.ui :as uir]))

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
                            (deref :ClickCount) ". "
                            [:input {:type "button" :value "Click me!"
                                     :on-click #(swap! :ClickCount inc)}]])}}})

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

(defn- test-01 []
  (let [evt (cn/make-instance {:BasicUI/ShowGreeting
                               {:Message "hello, world"}})
        result (e/eval-all-dataflows evt)]
    (println result)))

(f/component :EdnUI)

(def user-name (rg/atom ""))

(f/entity {:EdnUI/UserLogin
           {:UserNameLabel {:type :Kernel/String
                            :default "Username: "}
            :PasswordLabel {:type :Kernel/String
                            :default "Password: "}
            :ButtonTitle {:type :Kernel/String
                          :default "Login"}
            :HandlerEvent {:type :Kernel/Keyword
                           :optional true}
            :DOM_View {:type :Kernel/Edn
                       :default
                       [:div
                        [:div
                         [:label :UserNameLabel]
                         [:input {:type "text"
                                  :on-change #(reset! user-name (-> % .-target .-value))}]]
                        [:div
                         [:label :PasswordLabel]
                         [:input {:type "password"}]]
                        [:div
                         [:input {:type "button" :value :ButtonTitle
                                  :on-click [:HandlerEvent user-name]}]]]}}})

(f/entity {:EdnUI/LoginForm
           {:UserLogin {:type :EdnUI/UserLogin
                        :optional true}
            :Title {:type :Kernel/String
                    :default "Login"}
            :DOM_Target :Kernel/String
            :DOM_View {:type :Kernel/Edn
                       :default
                       [:div
                        [:h2 :Title]
                        [:div :UserLogin]]}}})

(f/event {:EdnUI/LoginEvent
          {:EventObject :Kernel/Any
           :UserData :Kernel/Any}})

(f/record {:EdnUI/LoginEventResult
           {:Result :Kernel/Any}})

(f/dataflow :EdnUI/MakeLoginForm
            {:EdnUI/UserLogin
             {:UserNameLabel :EdnUI/MakeLoginForm.UserNameLabel
              :PasswordLabel :EdnUI/MakeLoginForm.PasswordLabel
              :ButtonTitle :EdnUI/MakeLoginForm.ButtonTitle
              :HandlerEvent :EdnUI/MakeLoginForm.HandlerEvent}}
            {:EdnUI/LoginForm
             {:Title :EdnUI/MakeLoginForm.FormTitle
              :DOM_Target "app"
              :UserLogin :EdnUI/UserLogin}})

(f/dataflow :EdnUI/LoginEvent
            {:EdnUI/LoginEventResult
             {:Result :EdnUI/LoginEvent.UserData}})

(rr/override-resolver
 :EdnUI/LoginForm
 (uir/make-resolver :Edn-Resolver-01))

(defn test-02 []
  (let [evt (cn/make-instance {:EdnUI/MakeLoginForm
                               {:FormTitle "Login to the V8 Platform"
                                :UserNameLabel "Your V8 userId or email: "
                                :PasswordLabel "Password: "
                                :ButtonTitle "Login"
                                :HandlerEvent :EdnUI/LoginEvent}})
        result (doall (e/eval-all-dataflows evt))]
    result))

(test-02)
