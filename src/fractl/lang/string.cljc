(ns fractl.lang.string
  #?(:clj
     (:import [org.mindrot BCrypt])))

(defn string-in-range? [min max obj]
  (and (string? obj)
       (<= min (count obj) max)))

(defn password-encrypt
  ([s work-factor]
  #?(:clj
     (BCrypt/hashpw s (BCrypt/gensalt work-factor))
     :cljs s))
  ([s] (password-encrypt s 10)))

(defn password-check [s hashed]
  #?(:clj
     (BCrypt/checkpw s hashed)
     :cljs (= s hashed)))
