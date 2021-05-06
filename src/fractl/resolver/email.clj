(ns fractl.resolver.email
  "Fractl resolver to send email."
  (:require [sendgrid.core :as sendgrid]
            [fractl.resolver.core :as r]))

(defn api [key]
  {:api-key key})

(defn send-email
  "Simple send email using simple HTML."
  [key receiver sender subject text]
  (sendgrid/send-email (api key) {:to      receiver
                                  :from    sender
                                  :subject subject
                                  :html    "<h1> " (str text) " </h1>"}))

(defn- email [inst]
  (let [key (:Key inst)
        receiver (:Receiver inst)
        sender (:Sender inst)
        subject (:Subject inst)
        text (:Text inst)]
    (send-email key receiver sender subject text)))

(def ^:private resolver-fns
  {:eval {:handler email}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
