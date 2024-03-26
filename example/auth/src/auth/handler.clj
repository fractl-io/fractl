(ns auth.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [clojure.pprint :as pp]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.params :refer [wrap-params]]
            [hiccup2.core :as h]
            [org.httpkit.client :as http]
            [cheshire.core :as json]))

(defn- render [hiccup]
  (str (h/html hiccup)))

(defn- home []
  [:div
   [:form {:action "/auth"}
    [:input {:type "submit" :value "Place Order"}]]])

(def sid (atom nil))

(defn order []
  [:div
   [:div [:h2 "New Order"]]
   [:form {:action "/api/Twist/Order" :method :post}
    [:table
     [:tr [:td "Order Id"] [:td [:input {:type "text" :id "order_id" :name "order_id"}]]]
     [:tr [:td "Customer Id"] [:td [:input {:type "text" :id "customer_id" :name "customer_id"}]]]
     [:tr [:td "Email"] [:td [:input {:type "text" :id "email" :name "email"}]]]
     [:tr [:td "Qty"] [:td [:input {:type "text" :id "order_qty" :name "order_qty"}]]]
     [:tr [:td "Amount"] [:td [:input {:type "text" :id "order_amount" :name "order_amount"}]]]
     [:tr [:td] [:td]]
     [:tr [:td [:input {:type "submit" :value "Submit"}]] [:td]]]]])

(defn- insts-as-table [insts]
  (let [insts (mapv (fn [inst] (dissoc inst "-*-type-*-" "type-*-tag-*-")) insts)
        ks (keys (first insts))
        header (str (reduce (fn [s h] (str s "<th>" h "</th>")) "<tr>" ks) "</tr>")]
    (str
     (reduce (fn [s inst]
               (let [vs (mapv #(get inst %) ks)]
                 (str s (reduce (fn [s v] (str s "<td>" v "</td>")) "<tr>" vs) "</tr>")))
             (str "<table>" header) insts)
     "</table>")))

(defroutes app-routes
  (GET "/" [] (render (home)))
  (GET "/order" [] (render (order)))
  (GET "/auth" request (let [result (dissoc
                                     @(http/request {:method :get
                                                     :url "http://localhost:8000/auth"
                                                     :follow-redirects false})
                                     :opts)]
                         {:status 302
                          :headers {"Location" (get-in result [:headers :location])}}))
  (GET "/auth/callback" request (let [url (str "http://localhost:8000/auth/callback?" (:query-string request))
                                      result (dissoc @(http/request {:method :get :url url :follow-redirects false}) :opts)
                                      headers (:headers result)]
                                  (reset! sid (:set-cookie headers))
                                  {:status 302
                                   :headers {"Location" (:location headers)}}))
  (POST "/api/Twist/Order" request (let [params (:params request)
                                         attrs (assoc params "order_qty" (read-string (get params "order_qty"))
                                                      "order_amount" (read-string (get params "order_amount")))
                                         inst {"Twist/Order" attrs}
                                         url "http://localhost:8000/api/Twist/Order"
                                         result @(http/request {:method :post :url url
                                                                :headers {"Content-Type" "application/json"
                                                                          "Cookie" @sid}
                                                                :body (json/generate-string inst)})]
                                         (if (= 200 (:status result))
                                           {:status 302
                                            :headers {"Location" "/api/Twist/Order"}}
                                           {:status (:status result)
                                            :body (:body result)
                                            :headers {"Content-Type" "text/html"}})))
  (GET "/api/Twist/Order" request (let [url "http://localhost:8000/api/Twist/Order"
                                        result @(http/request {:method :get :url url
                                                               :headers {"Content-Type" "application/json"
                                                                         "Cookie" @sid}})]
                                    (if (= 200 (:status result))
                                      {:status 200
                                       :body (str
                                              "<html><body>"
                                              (insts-as-table (get (first (json/parse-string (:body result))) "result"))
                                              "<a href=\"/order\">Place new order</a>"
                                              "</body></html>")
                                       :headers {"Content-Type" "text/html"}}
                                      {:status (:status result)
                                       :body (:body result)
                                       :headers {"Content-Type" "text/html"}})))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-params
      wrap-multipart-params))
