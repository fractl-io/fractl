(ns fractl.resolver.registry
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.resolver.remote :as remote]
            [fractl.resolver.policy :as policy]
            [fractl.resolver.auth :as auth]
            #?(:clj [fractl.resolver.data-sync :as ds])
            #?(:clj [fractl.resolver.git :as git])
            #?(:clj [fractl.resolver.email :as email])
            #?(:clj [fractl.resolver.sms :as sms])))

(def ^:private resolver-db (u/make-cell {}))

(defn resolver-for-path
  ([resolver path]
   (get resolver (li/split-path path)))
  ([path]
   (resolver-for-path @resolver-db path)))

(defn override-resolver [path resolver]
  (if (vector? path)
    (doseq [p path] (override-resolver p resolver))
    (u/call-and-set
     resolver-db
     #(assoc
       @resolver-db
       (li/split-path path) resolver))))

(defn compose-resolver [path resolver]
  (if (vector? path)
    (doseq [p path] (compose-resolver p resolver))
    (let [path (li/split-path path)
          resolvers (get @resolver-db path [])]
      (u/call-and-set
       resolver-db
       #(assoc
         @resolver-db path
         (conj resolvers resolver))))))

(def composed? (complement map?))
(def override? map?)

(def constructors
  (u/make-cell
   (merge {:remote remote/make
           :policy policy/make
           :auth auth/make}
          #?(:clj {:data-sync ds/make})
          #?(:clj {:git git/make})
          #?(:clj {:email email/make})
          #?(:clj {:sms sms/make}))))

(defn register-resolver-type [type-name constructor]
  (u/call-and-set
   constructors
   #(assoc @constructors type-name constructor)))

(defn register-resolver [{n :name t :type
                          compose? :compose?
                          paths :paths config :config}]
  (if-let [c (t @constructors)]
    (let [resolver (c n config)
          rf (if compose? compose-resolver override-resolver)]
      (doseq [p paths] (rf p resolver))
      n)
    (u/throw-ex (str "Invalid resolver type " t " for resolver " n))))

(defn register-resolvers [specs]
  (doall (map register-resolver specs)))

(defn registered-resolvers
  []
  @resolver-db)
