(ns fractl.inference.util
  (:import pjstadig.Assertions)
  (:require [fractl.util :as u]
            [malli.core :as malli]
            [malli.transform :as mallt]))

(defn make-validator-explainer [schema & [options]]
  (let [vf? (malli/validator schema)
        {:keys [validate?]
         :or {validate? Assertions/assertionsEnabled}} options]
    (if validate?
      (with-meta
        (fn [supplied-value]
          (let [value (malli/decode schema supplied-value mallt/default-value-transformer)]
            (if (vf? value)
              value
              (let [explanation (malli/explain schema value)]
                (throw (ex-info (str "Validation error: \n" (u/pretty-str explanation))
                                {:explanation explanation
                                 :value value}))))))
        {:type :validator-explainer})
      (with-meta
        (fn [supplied-value]
          (malli/decode schema supplied-value mallt/default-value-transformer))
        {:type :decoder}))))
