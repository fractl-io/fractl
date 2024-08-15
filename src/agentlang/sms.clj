(ns agentlang.sms
  (:require [org.httpkit.client :as http]
            [agentlang.util :as u]))

(def base "https://api.twilio.com/2010-04-01")

(defn make-request-url [sid endpoint]
  (format "%s/Accounts/%s/SMS/%s.json"
          base
          sid
          endpoint))

(defn request
  "Make a simple POST request to the API"
  [method url sid token & params]
  (let [request-params (into {} params)]
    (try
      @(http/request
        {:method method
         :url url
         :form-params request-params
         :basic-auth [sid token]
         :socket-timeout 3000
         :conn-timeout 3000}) ; Timeout the request in 3 seconds
      (catch Exception e
        {:error e}))))

(defn send-sms
  "Send an SMS message which is a map in the form {:From x :To x :Body x}"
  [params]
  (let [sid (u/getenv "TWILIO_SID")
        token (u/getenv "TWILIO_AUTH_TOKEN")
        url (make-request-url sid "Messages")]
    (request :post url sid token params)))

