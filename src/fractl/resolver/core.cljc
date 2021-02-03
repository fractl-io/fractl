(ns fractl.resolver.core
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]))

(def ^:private valid-resolver-keys #{:upsert :delete :get :query :eval :preprocess})

(defn make-resolver
  ([resolver-name fnmap eval-dataflow]
  (when-not (every? identity (map #(some #{%} valid-resolver-keys) (keys fnmap)))
    (u/throw-ex (str "invalid resolver keys - " (keys fnmap))))
  (doseq [[k v] fnmap]
    (when-not (fn? (:handler v))
      (u/throw-ex (str "resolver key " k " must be mapped to a function"))))
  (assoc fnmap
         :name resolver-name
         :evt-handler eval-dataflow))
  ([resolver-name fnmap]
   (make-resolver resolver-name fnmap nil)))

(def resolver-name :name)
(def resolver-upsert :upsert)
(def resolver-delete :delete)
(def resolver-query :query)
(def resolver-eval :eval)
(def resolver-preprocess :preprocess)

(def can-preprocess? :preprocess)

(defn- ok? [r] (= :ok (:status r)))

(defn- ok-ffresult [r]
  (when (ok? r)
    (ffirst (:result r))))

(defn- apply-xform
  [xform eval-dataflow arg]
  (cond
    (fn? xform)
    (xform arg)

    (li/name? xform)
    (when eval-dataflow
      (let [evt-inst (cn/make-instance
                      {xform {:Instance arg}})
            result (eval-dataflow evt-inst)]
        (ok-ffresult (first result))))

    :else
    arg))

(defn- apply-xforms
  [xforms eval-dataflow arg]
  (loop [xforms xforms arg arg]
    (if-let [xf (first xforms)]
      (recur (rest xforms)
             (apply-xform xf eval-dataflow arg))
      arg)))

(defn- invoke-method [method resolver f arg]
  (if-let [in-xforms (get-in resolver [method :xform :in])]
    (let [eval-dataflow (:evt-handler resolver)
          final-arg (apply-xforms in-xforms eval-dataflow arg)
          result (f final-arg)]
      (if-let [out-xforms (get-in resolver [method :xform :out])]
        (apply-xforms out-xforms eval-dataflow result)
        result))
    (f arg)))

(defn- wrap-result [method resolver arg]
  (when-let [m (get-in resolver [method :handler])]
    {:resolver (:name resolver)
     :method method
     :result (invoke-method method resolver m arg)}))

(def call-resolver-upsert (partial wrap-result :upsert))
(def call-resolver-delete (partial wrap-result :delete))
(def call-resolver-query (partial wrap-result :query))
(def call-resolver-eval (partial wrap-result :eval))
(def call-resolver-preprocess (partial wrap-result :preprocess))
