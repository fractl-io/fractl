(ns fractl.auth.jwt
  (:require [fractl.datafmt.json :as json])
  #?(:clj (:import [org.jose4j.jwt JwtClaims]
                   [org.jose4j.jwt.consumer JwtConsumer JwtConsumerBuilder])))

(defn decode [token]
  #?(:clj
     (let [^JwtConsumer consumer (-> (JwtConsumerBuilder.)
                                     (.setSkipAllValidators)
                                     (.setDisableRequireSignature)
                                     (.setSkipSignatureVerification)
                                     (.build))
           ^JwtClaims claims (.processToClaims consumer token)]
       (json/decode (.getRawJson claims)))))
