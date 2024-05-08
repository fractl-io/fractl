(ns fractl.lang.b64
  #?(:clj
     (:import [java.util Base64])
     :cljs
     (:require [goog.crypt.base64 :as b64])))

(defn encode [bytes]
  #?(:clj
     (.encodeToString (Base64/getEncoder) bytes)
     :cljs
     (b64/encodeByteArray bytes)))

(def encode-string
  #?(:clj
     (fn [^String s]
       (encode (.getBytes s "UTF-8")))
     :cljs encode))

(defn decode [s]
  #?(:clj
     (.decode (Base64/getDecoder) s)
     :cljs
     (b64/decodeStringToByteArray s)))

(defn decode-to-string [s]
  #?(:clj (String. (decode s) "UTF-8")
     :cljs (b64/decodeString s false)))

(def ^:private b64-pat #"^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$")

(def base64-encoded? (partial re-matches b64-pat))
