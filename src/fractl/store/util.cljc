(ns fractl.store.util
  (:require #?(:clj [cheshire.core :as json])
            [fractl.component :as cn]))

(defn normalize-connection-info [connection-info]
  (if-let [f (:decrypt-fn connection-info)]
    (let [pswd (f (:password connection-info))]
      (assoc connection-info :password pswd))
    connection-info))

(defn result-as-instance [entity-name id-key json-key result]
  (let [id (id-key result)
        json-str #?(:clj  (String. (json-key result))
                    :cljs (str (json-key result)))
        parsed-obj (assoc #?(:clj (json/parse-string json-str true)
                             :cljs (cljs.reader/read-string json-str))
                          :Id (str id))]
    (cn/make-instance entity-name parsed-obj)))

(defn results-as-instances [entity-name id-key json-key results]
  (doall (map (partial result-as-instance entity-name id-key json-key) results)))

(defn make-result-keys [entity-name]
  (let [n (name (second entity-name))]
    [(keyword (str n "/ID")) (keyword (str n "/INSTANCE_JSON"))]))

(defn clj->json
  [data]
  #?(:clj (json/generate-string data)
     :cljs (.stringify js/JSON (clj->js data))))
