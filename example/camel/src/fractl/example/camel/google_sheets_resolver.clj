(ns fractl.example.camel.google-sheets-resolver
  (:require [selmer.parser :as st]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.datafmt.json :as json]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.evaluator.camel :as camel])
  (:import [java.util Arrays]
           [org.apache.camel Component]
           [org.apache.camel.component.google.sheets GoogleSheetsComponent
            GoogleSheetsConfiguration]
           [com.google.api.services.sheets.v4.model Spreadsheet ValueRange]))

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

(defn- as-values [xss]
  (mapv #(Arrays/asList (into-array %)) xss))

(def ^:private endpoint-templates
  {:create-sheet "google-sheets://spreadsheets/create?inBody=content"})

(defn- gs-create [camel-component instance]
  (let [[_ n] (li/split-path (cn/instance-type instance))]
    (case n
      :Sheet
      (let [ep (:create-sheet endpoint-templates)
            result (json/decode
                    (camel/exec-route {:endpoint ep
                                       :user-arg (Spreadsheet.)
                                       :user-arg-type Spreadsheet
                                       :camel-component camel-component}))]
        (assoc instance
               :Properties (:properties result)
               :Id (:spreadsheetId result)
               :Url (:spreadsheetUrl result)))
      :Data
      (let [range (:Range instance)
            ep (str "google-sheets://data/append?spreadsheetId=" (:SheetId instance) "&range=" range "&inBody=values")
            data (Arrays/asList (into-array (as-values (:Values instance))))
            ^ValueRange values (ValueRange.)]
        (.setValues values data)
        (when-let [result (camel/exec-route {:endpoint ep
                                             :user-arg values
                                             :user-arg-type ValueRange
                                             :headers {"CamelGoogleSheets.ValueInputOption" "USER_ENTERED"}
                                             :camel-component camel-component})]
          (let [r (json/decode result)]
            (when (= (:SheetId instance) (:spreadsheetUrl r))
              instance)))))))

(rg/register-resolver-type
 :camel-google-sheets
 (fn [_ _]
   (let [c (gs-make-component)]
     (r/make-resolver
      :camel-google-sheets
      {:create (partial gs-create c)}))))
