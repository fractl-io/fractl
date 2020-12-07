(ns fractl.lang.b64
  #?(:clj
     (:import [java.util Base64])))

(defn encode [bytes]
  #?(:clj
     (.encodeToString (Base64/getEncoder) bytes)))

(defn decode [s]
  #?(:clj
     (.decode (Base64/getDecoder) s)))

(def ^:private b64-pat #"^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)?$")

(def base64-encoded? (partial re-matches b64-pat))
