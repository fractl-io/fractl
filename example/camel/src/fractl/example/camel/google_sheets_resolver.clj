(ns fractl.example.camel.google-sheets-resolver
  (:require [selmer.parser :as st]
            [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.evaluator.camel :as camel])
  (:import [org.apache.camel Component]
           [org.apache.camel.component.google.sheets GoogleSheetsComponent
            GoogleSheetsConfiguration]
           [com.google.api.services.sheets.v4.model Spreadsheet]))

(defn ^Component gs-make-component []
  (let [^GoogleSheetsComponent gsc (GoogleSheetsComponent.)
        ^GoogleSheetsConfiguration config (GoogleSheetsConfiguration.)]
    (.setAccessToken config (u/getenv "GOOGLE_SHEETS_ACCESS_TOKEN"))
    (.setRefreshToken config (u/getenv "GOOGLE_SHEETS_REFRESH_TOKEN"))
    (.setClientId config (u/getenv "GOOGLE_SHEETS_CLIENT_ID"))
    (.setClientSecret config (u/getenv "GOOGLE_SHEETS_CLIENT_SECRET"))
    (.setApplicationName config "fractl")
    (.setConfiguration gsc config)
    gsc))

(def ^:private endpoint-templates
  {:create "google-sheets://spreadsheets/create?inBody=content"})

(defn- gs-create [camel-component instance]
  (let [ep (:create endpoint-templates)
        result (camel/exec-route {:endpoint ep
                                  :user-arg (Spreadsheet.)
                                  :user-arg-type Spreadsheet
                                  :camel-component camel-component})]
    ;; TODO: process result.
    instance))

;; TODO implement :update to handle updating a spreadsheet.

(rg/register-resolver-type
 :camel-google-sheets
 (fn [_ _]
   (let [c (gs-make-component)]
     (r/make-resolver
      :camel-google-sheets
      {:create (partial gs-create c)}))))
