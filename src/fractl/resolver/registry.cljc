(ns fractl.resolver.registry
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.resolver.remote :as remote]
            [fractl.resolver.policy :as policy]
            [fractl.resolver.auth :as auth]
            [fractl.resolver.timer :as timer]
            #?(:clj [fractl.resolver.data-sync :as ds])
            #?(:clj [fractl.resolver.git :as git])
            #?(:clj [fractl.resolver.email :as email])
            #?(:clj [fractl.resolver.sms :as sms])))

(def ^:private type-tag :-*-resolver-registry-*-)
(def ^:private parent-tag :-*-parent-*-)

(defn make
  ([parent]
   (u/make-cell {parent-tag parent type-tag true}))
  ([] (make nil)))

(defn registry? [obj]
  (and (u/cell? obj)
       (type-tag @obj)))

(def ^:private root-resolver-db (make))

(defn resolver-for-path
  ([resolver-db path]
   (when resolver-db
     (let [k (li/split-path path)]
       (loop [db @resolver-db]
         (if-let [r (get db k)]
           r
           (when-let [p (parent-tag db)]
             (recur @p)))))))
  ([path]
   (resolver-for-path root-resolver-db path)))

(defn override-resolver
  ([resolver-db path resolver]
   (if (vector? path)
     (doseq [p path] (override-resolver p resolver))
     (u/call-and-set
      resolver-db
      #(assoc
        @resolver-db
        (li/split-path path) resolver))))
  ([path resolver]
   (override-resolver root-resolver-db path resolver)))

(defn compose-resolver
  ([resolver-db path resolver]
   (if (vector? path)
     (doseq [p path] (compose-resolver p resolver))
     (let [path (li/split-path path)
           resolvers (get @resolver-db path [])]
       (u/call-and-set
        resolver-db
        #(assoc
          @resolver-db path
          (conj resolvers resolver))))))
  ([path resolver]
   (compose-resolver root-resolver-db path resolver)))

(def composed? (complement map?))
(def override? map?)

(def constructors
  (u/make-cell
   (merge {:remote remote/make
           :policy policy/make
           :auth auth/make
           :timer timer/make}
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
  (doall (map register-resolver specs))
  root-resolver-db)

(defn root-registry
  []
  root-resolver-db)
