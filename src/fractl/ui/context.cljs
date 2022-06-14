(ns fractl.ui.context
  (:require [goog.net.cookies :as cookies]
            [fractl.component :as cn]
            [fractl.lang :as l]
            [fractl.lang.internal :as li]))

(defn- owner
  ([k obj]
   (when (l/kernel-auth-name? k)
     (l/auth-owner obj)))
  ([obj]
   (owner (li/split-path (cn/instance-type obj)) obj)))

(def ^:private db-key "fractl-ui-context")
(def ^:private has-local-storage (.-localStorage js/window))

(defn- fetch-db []
  (cljs.reader/read-string
   (or
    (if has-local-storage
      (.getItem (.-localStorage js/window) db-key)
      (cookies/get db-key))
    {})))

(defn- spit-db! [db]
  (if has-local-storage
    (.setItem (.-localStorage js/window) db-key (str db))
    (cookies/set db-key (str db))))

(defn- assoc-to-context [db obj]
  (if obj
    (let [obj1 (if-let [t (:transition obj)]
                 (:to t)
                 obj)
          [c n :as k] (li/split-path (cn/instance-type obj1))
          obj2 (if has-local-storage obj1 (cn/compact-instance obj1))]
      (assoc db n obj2 k obj2))
    db))

(defn attach-to-context!
  ([obj is-auth-response]
   (let [db1 (assoc-to-context (fetch-db) obj)
         db2 (if is-auth-response
               (assoc-to-context (assoc db1 :auth obj) (owner obj))
               db1)]
     (spit-db! db2)))
  ([obj]
   (attach-to-context! obj false)))

(defn hard-reset-context! []
  (spit-db! {}))

(defn context-as-map
  ([keyword-names-only]
   (let [db (fetch-db)
         c (dissoc db :auth)]
     (if keyword-names-only
       (into
        {}
        (filter (fn [[k _]]
                  (keyword? k))
                c))
       c)))
  ([] (context-as-map true)))

(defn lookup-by-name [n]
  (get (fetch-db) n))

(defn remove-by-name [n]
  (let [db (fetch-db)]
    (spit-db! (dissoc db n))))

(def ^:private active-inst-key :active-instance)

(defn set-active-instance! [obj]
  (let [db (fetch-db)]
    (spit-db! (assoc db active-inst-key obj))))

(defn lookup-active-instance []
  (get (fetch-db) active-inst-key))

(defn lookup-auth []
  (:auth (fetch-db)))

(defn admin-auth? []
  (= "admin" (get-in (lookup-auth) [:Owner :Name])))
