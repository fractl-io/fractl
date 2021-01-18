(ns fractl.datafmt.transit
  (:require [cognitect.transit :as t])
  #?(:clj
     (:import [java.io ByteArrayInputStream ByteArrayOutputStream])))

(defn encode [obj]
  #?(:clj
     (let [out (ByteArrayOutputStream. 4096)
           w (t/writer out :json)]
       (t/write w obj)
       (.toString out))
     :cljs
     (let [w (t/writer :json)]
       (t/write w obj))))

(defn decode [s]
  #?(:clj
     (let [in (ByteArrayInputStream.
               (.getBytes s "UTF-8"))
           r (t/reader in :json)]
       (t/read r))
     :cljs
     (let [r (t/reader :json)]
       (t/read r s))))
