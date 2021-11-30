(ns fractl.util.hash
  (:require [clojure.string :as string])
  #?(:clj
     (:import [java.security SecureRandom]
              [java.security.spec KeySpec]
              [javax.crypto SecretKeyFactory]
              [javax.crypto.spec PBEKeySpec]
              [java.util Base64]
              [org.mindrot.jbcrypt BCrypt])))

(defn- salt []
  #?(:clj
     (let [^SecureRandom random (SecureRandom.)
           bs (byte-array 16)]
       (.nextBytes random bs)
       bs)))

(def ^:private default-iters 65536)
(def ^:private default-keylen 128)

(def ^:private hash-prefix "_v8sh__")
(def ^:private bcrypt-hash-prefix "_v8bsh__:")
(def ^:private hash-start-offset (count hash-prefix))

(defn salted-hash
  "Generate a random salt, hash the string with it.
      iters - the number of iterations for slowing down the hash fn.
      keylen - length (in bits) of the generated hash.
      salt - the salt used for hashing, byte-array.
   Return the string - b64_salt:b64_hash-of-s"
  ([^String s salt iters keylen]
   #?(:clj
      (let [^KeySpec spec (PBEKeySpec. (.toCharArray s) salt iters keylen)
            ^SecretKeyFactory factory (SecretKeyFactory/getInstance "PBKDF2WithHmacSHA1")
            pwd-hash (.getEncoded (.generateSecret factory spec))
            b64e (Base64/getEncoder)]
        (str hash-prefix (.encodeToString b64e salt) ":" (.encodeToString b64e pwd-hash)))
      :cljs
      s )) ;; no hashing in cljs
  ([s salt] (salted-hash s salt default-iters default-keylen))
  ([s] (salted-hash s (salt) default-iters default-keylen)))

(defn salted-hash? [x]
  (and (string? x)
       (= 0 (string/index-of x hash-prefix))))

(defn bcrypt-hash
  "Generate a bcrypt hash of a string with optional argument
  for number of iterations.

  iters - number of iterations for the salt
   Return the string - <bcrypt-hash-prefix><bcrypt-hash-of-s>  
  "
  ([s] #?(:clj (str bcrypt-hash-prefix (BCrypt/hashpw s (BCrypt/gensalt)))))
  ([s iters] #?(:clj (str bcrypt-hash-prefix (BCrypt/hashpw s (BCrypt/gensalt iters))))))

(defn bcrypt-hash? [x]
  (and (string? x)
       (= 0 (string/index-of x bcrypt-hash-prefix))))

(defn- extract-salt [s-hash]
  #?(:clj
     (when-let [i (string/index-of s-hash \:)]
       (let [^String s (subs s-hash hash-start-offset i)]
         (.decode (Base64/getDecoder) s)))))

(defn- extract-bcrypt-prefix [s-hash]
  #?(:clj
     (when-let [i (string/index-of s-hash \:)]
       (subs s-hash (+ i 1)))))

(defn bcrypt-hash-eq?
  "Return true if the bcrypt hash for string `s` is a match for the hash `s-hash`"
  ([s-hash s] #?(:clj
                 (when-let [real-hash (extract-bcrypt-prefix s-hash)]
                   (BCrypt/checkpw s real-hash)))))
                       
(defn hash-eq?
  "Return true if the hash for `s` is the same as salted-hash."
  ([^String s-hash ^String s iters keylen]
   #?(:clj
      (when-let [salt (extract-salt s-hash)]
        (= s-hash (salted-hash s salt iters keylen)))
      :cljs (= s-hash s)))
  ([^String s-hash ^String s]
   (hash-eq? s-hash s default-iters default-keylen)))
