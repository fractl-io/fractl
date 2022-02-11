(ns fractl.resolver.meta
  "Dynamic model definition"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r])
  #?(:clj (:import [java.io File])))

(def ^:private models-root "models")
(def ^:private model-spec-keys #{:name :components :config})

(defn- validate-model [spec]
  (doseq [k (keys spec)]
    (when-not (some #{k} model-spec-keys)
      (u/throw-ex (str "invalid key in model spec - " k))))
  spec)

(defn- init-model [spec]
  (if-let [model-name (:name spec)]
    (let [spec (validate-model spec)]
      #?(:clj
         (let [dir (str models-root File/separator (name model-name))]
           (.mkdir (File. dir))
           (spit (str dir File/separator u/model-script-name) spec)))
      model-name)
    (u/throw-ex "failed to init model, name missing")))

(defn meta-upsert [fractl-api meta-inst]
  (let [t (u/string-as-keyword (:Type meta-inst))
        spec (:Spec meta-inst)]
    (if (= t :model)
      (init-model spec)
      (let [f (t fractl-api)]
        (if f
          (if (= t :dataflow)
            (apply f spec)
            (f spec))
          (u/throw-ex (str "upsert failed, invalid :Kernel/Meta.Type - " t)))))))

(defn- meta-delete [fractl-api meta-inst]
  ;; TODO: implement delete
  (:Id meta-inst))

(defn- meta-query [fractl-api query]
  ;; TODO: implement fractl type lookup
  nil)

(def ^:private resolver-fns
  {:upsert {:handler meta-upsert}
   :delete {:handler meta-delete}
   :query {:handler meta-query}})

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (let [fractl-api (:fractl-api config)]
    (r/make-resolver
     resolver-name
     {:upsert {:handler (partial meta-upsert fractl-api)}
      :delete {:handler (partial meta-delete fractl-api)}
      :query {:handler (partial meta-query fractl-api)}})))
