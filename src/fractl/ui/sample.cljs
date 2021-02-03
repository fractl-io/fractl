(ns fractl.ui.sample
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

(f/entity {:EdnUI/UserLoginInfo
           {:Name :Kernel/String
            :Password :Kernel/String}})

(f/entity {:EdnUI/UserLoginView
           {:UserNameLabel {:type :Kernel/String
                            :default "Username: "}
            :PasswordLabel {:type :Kernel/String
                            :default "Password: "}
            :ButtonTitle {:type :Kernel/String
                          :default "Login"}
            :HandlerEvent {:type :Kernel/Keyword
                           :optional true}
            :UserLoginInfo {:ref :EdnUI/UserLoginInfo.Id}
            :DOM_View {:type :Kernel/Edn
                       :default
                       [:div
                        [:div
                         [:label :UserNameLabel]
                         [:input {:type "text"
                                  :value :UserLoginInfo.Name
                                  :on-change [:set :UserLoginInfo.Name :value]}]]
                        [:div
                         [:label :PasswordLabel]
                         [:input {:type "password"
                                  :on-change [:set :UserLoginInfo.Password :value]}]]
                        [:div
                         [:input {:type "button" :value :ButtonTitle
                                  :on-click [:HandlerEvent :UserLoginInfo.Id]}]]]}}})

(f/entity {:EdnUI/LoginResultView
           {:UserLoginInfo :EdnUI/UserLoginInfo
            :DOM_Target :Kernel/String
            :DOM_View {:type :Kernel/Edn
                       :default
                       [:div
                        [:div [:label "Username: " [:b :UserLoginInfo.Name]]]
                        [:div [:label "Password: " [:b :UserLoginInfo.Password]]]]}}})

(f/entity {:EdnUI/LoginForm
           {:UserLoginView {:type :EdnUI/UserLoginView
                            :optional true}
            :Title {:type :Kernel/String
                    :default "Login"}
            :DOM_Target :Kernel/String
            :DOM_View {:type :Kernel/Edn
                       :default
                       [:div
                        [:h2 :Title]
                        [:div :UserLoginView]]}}})

(f/event {:EdnUI/LoginEvent
          {:EventObject :Kernel/Any
           :UserData :Kernel/Any}})

(f/record {:EdnUI/LoginEventResult
           {:Result :Kernel/Any}})

(f/dataflow :EdnUI/MakeLoginForm
            {:EdnUI/UserLoginInfo {:Name "" :Password ""}}
            {:EdnUI/UserLoginView
             {:UserNameLabel :EdnUI/MakeLoginForm.UserNameLabel
              :PasswordLabel :EdnUI/MakeLoginForm.PasswordLabel
              :ButtonTitle :EdnUI/MakeLoginForm.ButtonTitle
              :UserLoginInfo :EdnUI/UserLoginInfo.Id
              :HandlerEvent :EdnUI/MakeLoginForm.HandlerEvent}}
            {:EdnUI/LoginForm
             {:Title :EdnUI/MakeLoginForm.FormTitle
              :DOM_Target "app"
              :UserLoginView :EdnUI/UserLoginView}})

(f/event {:EdnUI/MakeLoginResultView
          {:UserLoginInfo :EdnUI/UserLoginInfo}})

(f/dataflow :EdnUI/MakeLoginResultView
            {:EdnUI/LoginResultView
             {:UserLoginInfo :EdnUI/MakeLoginResultView.UserLoginInfo
              :DOM_Target "app"}})

(f/dataflow :EdnUI/LoginEvent
            {:EdnUI/UserLoginInfo
             {:Id? :EdnUI/LoginEvent.UserData}}
            {:EdnUI/MakeLoginResultView
             {:UserLoginInfo :EdnUI/UserLoginInfo}})

(rr/override-resolver
 :EdnUI/LoginForm
 (uir/make-resolver :Edn-Resolver-01))

(rr/override-resolver
 :EdnUI/LoginResultView
 (uir/make-resolver :Edn-Resolver-02))

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
