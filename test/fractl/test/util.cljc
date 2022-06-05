(ns fractl.test.util
  (:require [fractl.evaluator :as e]
            #?(:clj  [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])
            [fractl.store :as store]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [cljc.java-time.local-date-time :as local-date-time]
            [cljc.java-time.local-date :as local-date]
            [cljc.java-time.local-time :as local-time]
            [cljc.java-time.zone-offset :refer [utc]]
            [cljc.java-time.month :as month]))

(defn- report-expected-ex [ex]
  (println (str "Expected exception in test: "
                #?(:clj (.getMessage ex)
                   :cljs ex)))
  ex)

(defn is-error [f]
  (is (try
        (do (f) false)
        #?(:clj  (catch Exception ex
                   (report-expected-ex ex))
           :cljs (catch js/Error e
                   (report-expected-ex e))))))

(defmacro defcomponent [component & body]
  `(do (fractl.lang/component ~component)
       ~@body
       ~component))

(defn fresult [r]
  (:result (first r)))

(defn nth-result [r n]
  (:result (nth r n)))

(defn embedded-results [r]
  (fresult (first (second r))))

(defn uuid-string []
  #?(:clj
     (str (java.util.UUID/randomUUID))
     :cljs
     (str (random-uuid))))

(defn first-result [evt]
  (first
   (fresult
    (e/eval-all-dataflows evt))))

(defn sleep [msec f]
  #?(:clj
     (do
       (try
         (Thread/sleep msec)
         (catch Exception ex
           nil))
       (f))
     :cljs
     (js/setTimeout f msec)))

(defn rand-str [len]
  #?(:clj  
     (apply str (take len (repeatedly #(char (+ (rand 26) 97)))))))

(defn rand-email [domain]
  #?(:clj  
     (str (rand-str 12) "@" domain)))

;; To test postgres in CI
;; export POSTGRES_ENABLED=<something>
;; To turn off
;; unset POSTGRES_ENABLED
(def test-with-postgres false)

(store/open-default-store
 #?(:clj (when test-with-postgres
           {:type     :postgres
            :host     (or (System/getenv "POSTGRES_HOST") "localhost")
            :dbname   (or (System/getenv "POSTGRES_DB") "postgres")
            :username (or (System/getenv "POSTGRES_USER") "postgres")
            :password (System/getenv "POSTGRES_PASSWORD")})
    :cljs {:type :alasql}))

(s/def ::past-and-future-date-time (s/int-in (local-date-time/to-epoch-second (local-date-time/of 2011 month/january 1 0 00 00) utc)
                                             (local-date-time/to-epoch-second (local-date-time/of 2030 month/december 31 23 59 59) utc)))

(s/def ::past-and-future-date (s/int-in (local-date/to-epoch-day (local-date/of 2011 month/january 1))
                                        (local-date/to-epoch-day (local-date/of 2030 month/december 31))))

(s/def ::time (s/int-in (local-time/to-nano-of-day (local-time/of 01 01))
                        (local-time/to-nano-of-day (local-time/of 23 59))))

(comment
  {:Kernel/UUID (list `s/with-gen string?
                      #(gen/fmap str (s/gen uuid?)))})

(def fractl-type->spec-clj-type
  {:Kernel/String string?
   :Kernel/Keyword keyword?
   :Kernel/Int int?
   :Kernel/Int64 int?
   :Kernel/BigInteger integer?
   :Kernel/Float float?
   :Kernel/Double double?
   :Kernel/Decimal #?(:clj decimal?
                      :cljs float?)
   :Kernel/Boolean boolean?
   :Kernel/Any any?
   :Kernel/Map map?
   :Kernel/UUID uuid?
   :Kernel/Path (list `s/or :string (list `s/and string? (complement clojure.string/blank?))
                      :keyword keyword?)
   :Kernel/Edn (list `s/or :vector vector?
                     :map map?
                     :symbol symbol?
                     :keyword keyword?
                     :string string?
                     :number number?
                     :boolean boolean?
                     :nil nil?
                     :list list?
                     :set set?)
   :Kernel/Date (list `s/with-gen (partial instance? java.time.LocalDate)
                      #(gen/fmap (fn [ms]
                                   (local-date/of-epoch-day ms))
                                 (s/gen ::past-and-future-date)))
   :Kernel/Time (list `s/with-gen (partial instance? java.time.LocalTime)
                      #(gen/fmap (fn [ms]
                                   (local-time/of-nano-of-day ms))
                                 (s/gen ::time)))
   :Kernel/DateTime (list `s/with-gen (partial instance? java.time.LocalDateTime)
                          #(gen/fmap (fn [ms]
                                       (local-date-time/of-epoch-second ms 0 utc))
                                     (s/gen ::past-and-future-date-time)))})

(defn get-spec-namespace [component-name entity-name]
  (keyword (str (name component-name) "/" (name entity-name))))

(defn get-spec-name [component-name entity-name var-name]
  (keyword (str (name component-name) "/" (name entity-name) "." (name var-name))))

(defn get-required-and-optional-keys [component-name entity-name all-keys component-meta]
  (let [required-keys (:required-attributes component-meta)
        optional-keys (clojure.set/difference (set all-keys) (set required-keys))
        required-keys-mapped (map #(get-spec-name component-name entity-name %) required-keys)
        optional-keys-mapped (map #(get-spec-name component-name entity-name %) optional-keys)]
    {:req required-keys-mapped
     :opt optional-keys-mapped}))

(defmulti define-spec :property-type)

;;String spec defn with format option
(comment
  ;;[com.gfredericks/test.chuck "0.2.13"] can be used for predefined regex patterns
  (defcomponent :RefCheck
                #_(entity {:RefCheck/E3 {:AIdId {:type :Kernel/String
                                               :format "^((19|2[0-9])[0-9]{2})-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$"}}}))
  (defmethod define-spec :Kernel/String [{:keys [spec-entity-name property-details]}]
    (let [spec-fn (if (:format property-details)
                    (list `s/def spec-entity-name (list `s/and string? #(re-matches (re-pattern (:format property-details)) %)))
                    (list `s/def spec-entity-name string?))]
      (eval spec-fn))))

(defmethod define-spec :oneof [{:keys [spec-entity-name property-details]}]
  (let [spec-fn (list `s/def spec-entity-name (:vals property-details))]
    (eval spec-fn)))

(defmethod define-spec :listof [{:keys [spec-entity-name property-details]}]
  (let [spec-fn (list `s/def spec-entity-name (list `s/coll-of (-> :listof-type property-details fractl-type->spec-clj-type)))]
    (eval spec-fn)))

(defmethod define-spec :default [{:keys [spec-entity-name property-type]}]
  (let [validator (or (fractl-type->spec-clj-type property-type)
                      property-type)
        spec-fn (list `s/def spec-entity-name validator)]
    (eval spec-fn)))

(defn- define-spec-and-eval [component-name entity-name entity-schema component-meta]
  (doseq [[p-name p-details] entity-schema]
    (let [spec-entity-name (get-spec-name component-name entity-name p-name)]

      (define-spec {:property-type (:type p-details)
                    :property-details p-details
                    :spec-entity-name spec-entity-name})))

  (let [spec-namespace (get-spec-namespace component-name entity-name)
        required-and-optional-keys (get-required-and-optional-keys component-name entity-name (keys entity-schema) component-meta)
        spec-fn (list `s/def spec-namespace (list `s/keys :req (:req required-and-optional-keys)
                                                  :opt (:opt required-and-optional-keys)))]
    (eval spec-fn)))

(defn fill-property-attributes [attr-schema]
  (cond
    (-> attr-schema :oneof seq) {:type :oneof
                                 :vals (-> attr-schema :oneof set)}
    (= (-> attr-schema :type) :Kernel/String) {:type :Kernel/String
                                               :format (-> attr-schema :format-str)}
    (-> attr-schema :listof some?) {:type :listof
                                    :listof-type (:listof attr-schema)}
    :else attr-schema))

(defn resolve-properties [entity-schema component-name]
  (reduce-kv (fn [r k v]
               (let [v-namespace (-> v namespace keyword)
                     attr-schema (fractl.component/find-attribute-schema v)
                     new-v (if (= v-namespace component-name)
                             (fill-property-attributes attr-schema)
                             {:type v})]
                 (assoc r k new-v)))
             {} entity-schema))

(defn maybe-assoc-id [schema]
  (if (:Id schema)
    (assoc schema :Id :Kernel/UUID)
    schema))

(defn get-deep-ref [prop-details component-name]
  (let [prop-type (if (-> prop-details :type (= :listof))
                    (:listof-type prop-details)
                    (:type prop-details))
        construct-deep-ref? (and (keyword? prop-type)
                                 (-> prop-type namespace keyword (= component-name)))]
    (when construct-deep-ref?
      prop-type)))

(defn construct-spec [component]
  (let [[component-name entity-name] (fractl.lang.internal/split-path component)
        component-meta (fractl.component/fetch-meta component)
        entity-schema (some-> component
                              fractl.component/fetch-schema
                              maybe-assoc-id
                              (resolve-properties component-name))
        _ (doseq [[_ prop-details] entity-schema]
            (when-let [deep-ref-type (get-deep-ref prop-details component-name)]
              (construct-spec deep-ref-type)))]
    (define-spec-and-eval component-name entity-name entity-schema component-meta)))

#?(:clj
   (defn generate-data [component]
     (let [_ (construct-spec component)
           [component-name entity-name] (fractl.lang.internal/split-path component)
           spec-name-space (get-spec-namespace component-name entity-name)]
       (gen/sample (s/gen spec-name-space)))))
