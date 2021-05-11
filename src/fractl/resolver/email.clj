(ns fractl.resolver.email
  "Fractl resolver to send email."
  (:require [sendgrid.core :as sendgrid]
            [postmark.core :as postmark]
            [fractl.resolver.core :as r]))

(defn api [key]
  {:api-key key})

(defn send-email
  "Simple send email using simple HTML.
  The backend option is either SendGrid or Postmark."
  [backend key receiver sender subject text]
  (if (= backend "SendGrid")
    (sendgrid/send-email (api key) {:to      receiver
                                    :from    sender
                                    :subject subject
                                    :html    "<h1> " (str text) " </h1>"})
    (let [pm (postmark/postmark key sender)]
      (pm {:to receiver
           :subject subject
           :text text}))))

(defn- email
  "The backend is either SendGrid or Postmark."
  [inst]
  (let [backend (:Backend inst)
        key (:Key inst)
        receiver (:Receiver inst)
        sender (:Sender inst)
        subject (:Subject inst)
        text (:Text inst)]
    (send-email backend key receiver sender subject text)))

(def ^:private resolver-fns
  {:eval {:handler email}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
