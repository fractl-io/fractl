(ns fractl.resolver.email
  "Fractl resolver to send email."
  (:require [sendgrid.core :as sendgrid]
            [postmark.core :as postmark]
            [fractl.resolver.core :as r]
            [fractl.util :as u]))

(defn send-email
  "Simple send email using simple HTML.
  The backend option is either SendGrid or Postmark."
  [backend receiver subject text]
  (if (= backend "SendGrid")
    (sendgrid/send-email {:api-key (u/getenv "EMAIL_API_KEY")}
                         {:to      receiver
                          :from    (u/getenv "EMAIL_SENDER")
                          :subject subject
                          :html    "<h1> " (str text) " </h1>"})
    (let [pm (postmark/postmark
               (u/getenv "EMAIL_API_KEY")
               (u/getenv "EMAIL_SENDER"))]
      (pm {:to receiver
           :subject subject
           :text text}))))

(defn- email
  "The backend is either SendGrid or Postmark."
  [inst]
  (let [backend (:Backend inst)
        receiver (:Receiver inst)
        subject (:Subject inst)
        text (:Text inst)]
    (send-email backend receiver subject text)))

(def ^:private resolver-fns
  {:eval {:handler email}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
