(ns fractl.util.hash
  (:require [clojure.string :as string])
  #?(:clj
     (:import [org.mindrot.jbcrypt BCrypt]
              [java.util Base64])))

(def ^:private hash-prefix "_fractlbsh__:")
(def ^:private fixed-salt "$2a$10$AaW/9iIw27WMW9C33n0aa.")

(defn crypto-hash? [x]
  (and (string? x)
       (= 0 (string/index-of x hash-prefix))))

(defn crypto-hash
  "Generate a (constant) cryptographic hash of a string. Algorithm used is bcrypt.
  Return the string - <hash-prefix><bcrypt-hash-of-s>

  Note: A constant salt is required to compare against DB values for queries
  otherwise the hash generated everytime is different and cant be used for
  query comparisons.  
  "
  ([^String s] #?(:clj
                  (let [b64e (Base64/getEncoder)]
                    (str hash-prefix
                         (.encodeToString b64e (.getBytes (BCrypt/hashpw s fixed-salt)))))
                  :cljs s)))

(defn crypto-hash-dynamic
  "Generate a (varyiable) cryptographic hash of a string. Algorithm used is bcrypt.
  Return the string - <hash-prefix><bcrypt-hash-of-s>

  Note: This uses a dynamic (variable) salt.
  "
  ([^String s] #?(:clj
                  (let [b64e (Base64/getEncoder)]
                    (str hash-prefix
                         (.encodeToString b64e (.getBytes (BCrypt/hashpw s (BCrypt/gensalt))))))
                  :cljs s)))

(defn- extract-prefix [s-hash]
  #?(:clj
     (when-let [i (string/index-of s-hash \:)]
       (apply str (map char (.decode (Base64/getDecoder) (.toString (subs s-hash (+ i 1))))
                       )))))

(defn crypto-hash-eq?
  "Return true if the cryptographic hash for string `s` is a match for the hash `s-hash`"
  ([^String s-hash ^String s] #?(:clj
                 (when-let [real-hash (extract-prefix s-hash)]
                   (BCrypt/checkpw s real-hash))
                 :cljs (= s-hash s))))
                       

(defn gensalt
  "Handy short-cut to generate a salt for testing"
  [] (BCrypt/gensalt))
