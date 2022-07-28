(ns fractl.resolver.timer
  (:require [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
            [fractl.evaluator.state :as es]
            [fractl.lang.datetime :as dt])
  #?(:clj
     (:import [java.util.concurrent ExecutorService Executors
               Future TimeUnit])))

(def ^:private db (u/make-cell {}))

#?(:clj
   (def ^:private ^ExecutorService executor (Executors/newCachedThreadPool)))

(defn- upsert-timer-inst [id created inst]
  (u/call-and-set
   db
   #(assoc
     @db id
     [created inst]))
  inst)

(defn- delete-timer-inst! [id]
  (u/call-and-set
   db
   #(dissoc @db id))
  id)

(defn- update-task-handle! [id handle]
  (when-let [[created inst]
             (get @db id)]
    (upsert-timer-inst
     id created (assoc inst :TaskHandle handle))))

(defn- expiry-as-ms [inst]
  (let [n (:Expiry inst)]
    (case (:ExpiryUnit inst)
      :Seconds (* 1000 n)
      :Minutes (* 60 1000 n)
      :Hours (* 60 60 1000 n)
      :Days (* 24 60 60 1000 n))))

(defn- sleep [inst]
  #?(:clj
     (try
       (let [eu (:ExpiryUnit inst)
             unit (if (keyword? eu)
                    eu
                    (keyword eu))]
         (.sleep
          (case unit
            :Seconds TimeUnit/SECONDS
            :Minutes TimeUnit/MINUTES
            :Hours TimeUnit/HOURS
            :Days TimeUnit/DAYS)
          (:Expiry inst)))
       (catch Exception ex
         (log/error (str "task sleep interrupted - " ex))))))

(defn- task-active? [id]
  (get @db id))

(defn- cancel-task! [id]
  (when-let [[_ inst] (get @db id)]
    (when-let [handle (:TaskHandle inst)]
      #?(:clj
         (.cancel ^Future handle true)
         :cljs
         (js/clearTimeout handle)))
    (delete-timer-inst! id)))

(defn- make-callback [id inst]
  (fn []
    (sleep inst)
    (when (task-active? id)
      (log/info (str "running timer task - " id))
      (let [result
            (try
              ((es/get-active-evaluator)
               (cn/make-instance (:ExpiryEvent inst)))
              (catch #?(:clj Exception :cljs js/Error) ex
                (log/error (str "error in task callback - " ex))))]
        (delete-timer-inst! id)
        result))))

(defn timer-upsert [inst]
  (let [id (cn/id-attr inst)]
    (upsert-timer-inst id (dt/now-raw) inst)
    (let [callback (make-callback id inst)
          handle #?(:clj
                    (.submit executor ^Callable callback)
                    :cljs
                    (js/setTimeout callback (expiry-as-ms inst)))]
      (update-task-handle! id handle)
      inst)))

(defn- timer-delete [arg]
  (let [id (r/id-to-delete arg)]
    (cancel-task! id)))

(def ^:private resolver-fns
  {:upsert {:handler timer-upsert}
   :delete {:handler timer-delete}})

(defmake :timer
  (fn [_ _]
    (r/make-resolver :timer resolver-fns)))
