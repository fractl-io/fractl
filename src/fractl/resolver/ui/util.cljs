(ns fractl.resolver.ui.util
  (:require [reagent.core :as r]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]))

(def ^:private global-render-view (atom nil))
(def ^:private remote-api-host (atom nil))

(defn set-global-render-view! [render-view]
  (reset! global-render-view render-view))

(defn get-global-render-view []
  @global-render-view)

(defn set-remote-api-host! [host]
  (reset! remote-api-host host))

(defn get-remote-api-host []
  @remote-api-host)

(def ^:private remote-eval-results (r/atom {}))

(defn get-remote-eval-results []
  @remote-eval-results)

(defn eval-result [result]
  (let [r (first result)]
    (if (= :ok (:status r))
      (:result r)
      (do (println "remote eval failed: " r) nil))))

(defn remote-eval-callback
  ([result-key result-proc result]
   (swap!
    remote-eval-results
    assoc result-key
    (or (result-proc (eval-result result)) :error)))
  ([result-key result]
   (remote-eval-callback result-key identity result)))

(defn eval-event
  ([callback eval-local event-instance]
   (if-let [host (and (not eval-local) @remote-api-host)]
     (do (ev/remote-evaluate host callback event-instance) nil)
     ((or callback identity) ((ev/global-dataflow-eval) event-instance))))
  ([callback event-instance]
   (eval-event callback false event-instance))
  ([event-instance]
   (eval-event identity event-instance)))

(defn eval-local-event [event-instance]
  (eval-event identity true event-instance))

(defn- upsert-event-name [entity-name]
  (let [[c n] (li/split-path entity-name)
        ev-name (keyword (str "Upsert_" (name n)))]
    (li/make-path c ev-name)))

(defn fire-upsert [entity-name object]
  (let [event-name (upsert-event-name entity-name)]
    (eval-event
     (when @remote-api-host
       (partial remote-eval-callback entity-name))
     (cn/make-instance
      {event-name
       {:Instance
        (if (cn/an-instance? object)
          object
          (cn/make-instance
           {entity-name object}))}}))))

(defn make-transformer
  ([recname schema]
   (fn [instance]
     (let [inst1
           (mapv
            (fn [[k v]]
              [k
               (let [tn (k schema)
                     t (if (k/kernel-type? tn) tn (:type (cn/find-attribute-schema tn)))]
                 (case t
                   (:Kernel/Int :Kernel/Int64 :Kernel/BigInteger) (js/parseInt v)
                   (:Kernel/Float :Kernel/Decimal :Kernel/Double) (js/parseFloat v)
                   :Kernel/Boolean (if (= v "false") false true)
                   v))])
            instance)]
       (cn/make-instance
        {recname
         (into {} inst1)}))))
  ([recname]
   (make-transformer recname (cn/fetch-schema recname))))

(defn assoc-input-value [place k evt]
  (swap! place assoc k (-> evt .-target .-value)))

(defn call-with-value [evt callback]
  (callback (-> evt .-target .-value)))
