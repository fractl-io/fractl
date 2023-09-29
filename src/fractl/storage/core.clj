(ns fractl.storage.core)

(def service-tag :service)

(defmulti make-client service-tag)
(defmulti create-storage service-tag)
(defmulti delete-storage service-tag)
(defmulti put-object-in-storage service-tag)
(defmulti delete-object-from-storage service-tag)
(defmulti give-user-access-to-storage service-tag)
(defmulti revoke-user-access-from-storage service-tag)
(defmulti download-object-from-storage service-tag)
(defmulti list-storages service-tag)
(defmulti list-objects-in-storage service-tag)

(def client-key :client)
(def instance-key :instance)



