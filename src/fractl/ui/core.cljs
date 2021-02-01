(ns fractl.ui.core
  (:require [clojure.walk :as w]
            [reagent.core :as rg]
            [reagent.dom :as rgdom]
            [fractl.lang :as f]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.store :as store]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.ui :as uir]))

(store/open-default-store {:type :reagent :reactive true})

(defn core-ui-upsert [inst]
  (rgdom/render
   [(:View inst)]
   (.getElementById js/document (:DOM_Target inst))))

(f/component :BasicUI)

(def click-count (rg/atom 0))

(f/entity {:BasicUI/Counter
           {:DOM_Target :Kernel/String
            :DOM_View {:type :Kernel/Edn
                       :default
                       [:div
                        "The atom " [:code "click-count"] " has value: "
                        @click-count ". "
                        [:input {:type "button" :value "Click me!"
                                 :on-click #(swap! click-count inc)}]]}}})

(rr/override-resolver
 :BasicUI/Counter
 (uir/make-resolver :Basic-UI-Resolver-01))

(defn- test-01 []
  (let [e (cn/make-instance {:BasicUI/Counter {:DOM_Target "app"}})
        evt (cn/make-instance {:BasicUI/Upsert_Counter
                               {:Instance e}})
        result (e/eval-all-dataflows evt)]
    (doall result)))

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
                                  :on-change [:set user-name :-value]}]]
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
        result (e/eval-all-dataflows evt)]
    (doall result)))

(test-02)
