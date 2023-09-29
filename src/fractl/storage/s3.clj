(ns fractl.storage.s3
  (:require
   [amazonica.aws.s3 :refer [create-bucket delete-bucket get-object put-object delete-object list-buckets list-objects-v2]]
   [fractl.component :as cn]
   [fractl.lang.internal :as li]
   [fractl.storage.core :as storage]
   [fractl.util.http :as uh])
  (:use [amazonica.core]))

(def ^:private tag :s3)

(defmethod storage/make-client tag [{:keys [access-key secret-key region] :as _config}]
  (defcredential (or (storage/client-key _config)
                     (access-key secret-key region))))

(defmethod storage/create-storage tag [{:keys [instance] :as req}]
  (let [{:keys [access-key secret-key region s3?] :as aws-config} (uh/get-aws-config)]
    (case (last (li/split-path (cn/instance-type instance)))
      :S3BucketCreate
      (let [bucket instance]
        (try
          (create-bucket (:Name bucket))
          (catch Exception e
            (println "Error creating bucket:" (.getMessage e))))))))

(defmethod storage/delete-storage tag [{:keys [instance] :as req}]
  (let [{:keys [access-key secret-key region s3?] :as aws-config} (uh/get-aws-config)]
    (case (last (li/split-path (cn/instance-type instance)))
      :S3BucketDelete
      (let [bucket instance]
        (try
          (delete-bucket (:Name bucket))
          (catch Exception e
            (println "Error deleting bucket:" (.getMessage e))))))))

(defmethod storage/put-object-in-storage tag [{:keys [instance] :as req}]
  (case (last (li/split-path (cn/instance-type instance)))
    :S3BucketPutObject
    (let [bucket instance]
      (try
        (put-object :bucket-name (:Name bucket)
                    :key (:ObjectKey bucket)
                    ;; WE use Server Side encryption for now.
                    ;; TODO support Client side encryption.
                    :metdata {:server-side-encryption "AES256"}
                    :file (:Content bucket))
        (catch Exception e
          (println "Error uploading object:" (.getMessage e)))))))

(defmethod storage/delete-object-from-storage tag [{:keys [instance] :as req}]
  (case (last (li/split-path (cn/instance-type instance)))
    :S3BucketDeleteObject
    (let [bucket instance]
      (try
        (delete-object :bucket-name (:Name bucket) :key (:ObjectKey bucket))
        (catch Exception e
          (println "Error uploading object:" (.getMessage e)))))))

(defn list-objects [bucket-name]
  (try
    (list-objects  bucket-name)
    (catch Exception e
      (println "Error listing objects:" (.getMessage e)))))

(defmethod storage/download-object-from-storage tag [{:keys [instance] :as req}]
  (case (last (li/split-path (cn/instance-type instance)))
    :S3BucketDownloadObject
    (let [bucket instance]
      (try
        (get-object (:Name bucket) (:ObjectKey bucket) (:LocalPath bucket))
        (catch Exception e
          (println "Error downloading object:" (.getMessage e)))))))

(defmethod storage/list-storages tag [{:keys [instance] :as req}]
  (case (last (li/split-path (cn/instance-type instance)))
    :S3BucketListStorages
    (let [bucket instance]
      (try
        (list-buckets {:client-config {:path-style-access-enabled false
                                       :chunked-encoding-disabled false
                                       :accelerate-mode-enabled false
                                       :payload-signing-enabled true
                                       :dualstack-enabled true
                                       :force-global-bucket-access-enabled true}})
        (catch Exception e
          (println "Error downloading object:" (.getMessage e)))))))

(defmethod storage/list-objects-in-storage tag [{:keys [instance] :as req}]
  (case (last (li/split-path (cn/instance-type instance)))
    :S3BucketListStorages
    (let [bucket instance]
      (try
        (list-objects-v2 {:bucket-name (:Name bucket)})
        (catch Exception e
          (println "Error downloading object:" (.getMessage e)))))))

#_(defn set-bucket-acl [bucket-name emails]
    (try
      (set-bucket-acl bucket-name :grant-read emails)
      (catch Exception e
        (println "Error setting bucket ACL:" (.getMessage e)))))

