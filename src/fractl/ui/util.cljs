(ns fractl.ui.util
  (:require [clojure.string :as s]
            [cognitect.transit :as t]
            [fractl.util :as u]
            [fractl.global-state :as gs]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.ui.meta :as mt]
            [fractl.ui.context :as ctx]))

(def ^:private remote-api-host (atom nil))
(def ^:private auth-required (atom false))
(def ^:private home-links (atom []))
(def ^:private ignore-in-home-links (atom []))

(defn set-authorization-required! [auth-rec-name]
  (reset! auth-required auth-rec-name))

(defn authorized! []
  (reset! auth-required false))

(defn auth-required? [] @auth-required)
(defn auth-rec-name [] @auth-required)

(defn clear-home-links! []
  (reset! ignore-in-home-links [])
  (reset! home-links []))

(defn attach-home-link! [s]
  (swap! home-links conj s))

(defn fetch-home-links []
  (let [ls @home-links
        igns @ignore-in-home-links]
    (mapv
     second
     (filter
      #(not (some #{(first %)} igns))
      ls))))

(defn ignore-in-home-links! [xs]
  (swap! ignore-in-home-links (comp set concat) xs))

(defn- as-url-path-name [n]
  (cond
    (keyword? n) (s/lower-case (name n))
    (string? n) n
    :else (str n)))

(def link-prefix "#")

(defn make-link [route-fn & args]
  (str link-prefix (apply route-fn args)))

(defn make-dashboard-route [n]
  (str "/" (as-url-path-name n)))

(defn make-list-view-route [n]
  (str "/" (as-url-path-name n) "/list"))

(defn make-instance-view-route
  ([n uq uv]
   (str
    "/" (as-url-path-name n) "/"
    (as-url-path-name uq)
    "/" (or uv ":s")))
  ([n uq]
   (make-instance-view-route n uq nil)))

(defn make-contains-route [n cn]
  (str "/" (as-url-path-name n) "/:id1/" (as-url-path-name cn) "/:id2"))

(defn set-remote-api-host! [host]
  (reset! remote-api-host host))

(defn get-remote-api-host []
  @remote-api-host)

(defn eval-result [result]
  (let [r (first result)]
    (if (= :ok (:status r))
      (:result r)
      (do (println "remote eval failed: " r) nil))))

(defn eval-event
  ([callback eval-local event-instance]
   (let [event-instance (assoc event-instance li/event-context
                               (ctx/context-as-map))]
     (if-let [host (and (not eval-local) @remote-api-host)]
       (do (ev/remote-evaluate host callback event-instance) nil)
       ((or callback identity) ((ev/global-dataflow-eval) event-instance)))))
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

(defn fire-upsert
  ([entity-name object callback]
   (let [event-name (upsert-event-name entity-name)]
     (eval-event
      callback
      (cn/make-instance
       {event-name
        {:Instance
         (if (cn/an-instance? object)
           object
           (cn/make-instance
            {entity-name object}))}}))))
  ([entity-name object]
   (fire-upsert entity-name object identity)))

(defn- delete-event-name [entity-name]
  (let [[c n] (li/split-path entity-name)]
    (str (name c) "/Delete_" (name n))))

(defn fire-delete-instance [entity-name id]
  (let [event-name (delete-event-name entity-name)]
    (eval-event
     #(println (str "delete " [entity-name id] " - " %))
     (cn/make-instance
      {event-name {:Id id}}))))

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

(def ^:private s-lookup-all "LookupAll")

(defn lookupall-event-name [rec-name]
  (keyword
   (if (string? rec-name)
     (str rec-name s-lookup-all)
     (let [[c n] (li/split-path rec-name)]
       (str (name c) "/" (name n) s-lookup-all)))))

(def ^:private fallback-render-event-names
  {:input :Fractl.UI/RenderGenericInputForm
   :instance :Fractl.UI/RenderGenericInstanceForm
   :list :Fractl.UI/RenderGenericTable
   :dashboard :Fractl.UI/RenderGenericDashboard})

(defn make-render-event [rec-name entity-spec tag meta]
  (let [spec-instance (:instance entity-spec)
        qinfo (:query-info entity-spec)
        qattrs (cond
                 spec-instance
                 {:Instance spec-instance}
                 qinfo
                 {:QueryBy (second qinfo)
                  :QueryValue (nth qinfo 2)}
                 :else {})
        tag (if (seqable? tag) tag [tag])
        tbl-attrs (case (first tag)
                    (:list :dashboard)
                    {:Source (or (:source entity-spec)
                                 (lookupall-event-name rec-name))}
                    nil)
        app-config (gs/get-app-config)]
    (if-let [event-name (mt/view-event meta tag)]
      (cn/make-instance event-name (merge qattrs tbl-attrs))
      (let [attrs {:Record rec-name
                   :Fields (:order meta)}]
        (cn/make-instance
         (get-in
          (or
           (get-in app-config [:ui :render-events rec-name])
           (get-in app-config [:ui :global-render-events])
           fallback-render-event-names)
          tag)
         (merge attrs qattrs tbl-attrs))))))

(def ^:private post-render-events (atom []))

(defn add-post-render-event! [event-fn]
  (swap! post-render-events conj event-fn))

(defn run-post-render-events! []
  (let [fns @post-render-events]
    (reset! post-render-events [])
    (doseq [f fns]
      (f))))

(def ^:private interval-handle (atom nil))

(defn clear-interval! []
  (when-let [h @interval-handle]
    (js/clearInterval h)))

(defn set-interval! [callback ms]
  (add-post-render-event!
   (fn []
     (clear-interval!)
     (reset!
      interval-handle
      (js/setInterval callback ms)))))

(defn reset-page-state! []
  (clear-interval!))

(defn decode-to-str [x]
  (if (t/tagged-value? x)
    (.-rep x)
    (str x)))

(defn- lookup-by-event-name [c n s]
  (keyword (str (name c) "/" (name n) "LookupBy" s)))

(defn make-query-event [rec-name query-by query-value]
  (let [[c n] (li/split-path rec-name)]
    (if (= :Id query-by)
      (cn/make-instance
       (keyword (str (name c) "/Lookup_" (name n)))
       {:Id query-value})
      (let [s (name query-by)]
        (cn/make-instance
         (lookup-by-event-name c n s)
         {query-by query-value})))))

(defn make-multi-arg-query-event [rec-name params]
  (let [[c n] (li/split-path rec-name)
        sfx (s/join "And" (mapv name (take-nth 2 params)))
        event-name (lookup-by-event-name c n sfx)]
    (cn/make-instance event-name (apply hash-map params))))

(defn make-multi-arg-query-event-spec [rec-name params]
  {:record rec-name
   :source (make-multi-arg-query-event rec-name params)})

(defn query-instance
  ([rec-name query-by query-value callback]
   (let [event-inst (make-query-event rec-name query-by query-value)]
     (eval-event
      (fn [r]
        (if-let [result (eval-result r)]
          (let [inst (first result)]
            (callback inst))
          (do (u/throw-ex
               (str "error: query-instance failed for "
                    [rec-name query-by query-value]
                    " - " r))
              (callback nil))))
      event-inst)))
  ([trigger-inst callback]
   (query-instance
    (:Record trigger-inst)
    (:QueryBy trigger-inst)
    (:QueryValue trigger-inst)
    callback)))

(defn ref-to-record [rec-name attr-scms]
  (let [n (li/split-path rec-name)]
    (first
     (filter
      identity
      (map
       #(let [sname (second %)
              parts (:ref (cn/find-attribute-schema sname))]
          (when (and parts (= n [(:component parts) (:record parts)]))
            [(first %) (:refs parts)]))
       attr-scms)))))

