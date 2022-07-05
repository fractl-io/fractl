(ns fractl.evaluator.intercept.instance-meta
  (:require [fractl.component :as cn]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.env :as env]
            [fractl.store :as store]
            [fractl.lang.internal :as li]
            [fractl.evaluator.intercept.internal :as ii]))

(defn- upsert-meta-failed-msg [entity-instances]
  (str "failed to upsert meta-data for "
       (cn/instance-type (first entity-instances))))

(defn- upsert-meta [env arg]
  (let [user (cn/event-context-user (ii/event arg))
        entity-instances (ii/data-output arg)]
    (if (and user entity-instances)
      (try
        (let [meta-infos (group-by
                        first
                        (mapv #(cn/make-meta-instance % user) entity-instances))
              ks (keys meta-infos)
              rs (mapv (fn [k]
                         (let [rows (mapv second (meta-infos k))]
                           (store/upsert-instances
                            (env/get-store env)
                            (li/split-path k) rows)))
                       ks)]
          (if (and (su/all-true? rs) (= (count rs) (count entity-instances)))
            entity-instances
            (log/error (upsert-meta-failed-msg entity-instances))))
        #?(:clj
           (catch Exception e
             (log/error (str (upsert-meta-failed-msg entity-instances)
                             " - " (or (ex-data e) (.getMessage e)))))
           :cljs
           (catch js/Error e
             (log/error (str (upsert-meta-failed-msg entity-instances)
                             " - " (or (.-ex-data e) (i/error e)))))))
      entity-instances)))

(defn- delete-meta [env arg]
  (if-let [[record-name id] (ii/data-output arg)]
    (try
      (store/delete-by-id
       (env/get-store env)
       (cn/meta-entity-name (li/split-path record-name)) id)
      #?(:clj
         (catch Exception e
           (log/error (str "delete meta error " [record-name id]
                           " - " (or (ex-data e) (.getMessage e)))))
         :cljs
         (catch js/Error e
           (log/error (str "delete meta error " [record-name id]
                           " - " (or (.-ex-data e) (i/error e)))))))
    arg))

(def ^:private actions
  {:upsert upsert-meta
   :delete delete-meta})

(defn- run [env opr arg]
  (if-let [f (opr actions)]
    (when (f env arg) arg)
    arg))

(defn make []
  (ii/make-interceptor :instance-meta run))
