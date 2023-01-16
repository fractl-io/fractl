(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.lang.tools.util :as tu]
            [fractl.lang.tools.loader :as loader])
  (:import [java.io File]
           [org.apache.commons.io FileUtils]
           [org.apache.commons.io.filefilter IOFileFilter WildcardFileFilter]))

(def cljout "cljout")
(def ^:private cljout-file (File. cljout))

(def ^:private logback-xml
  "<?xml version=\"1.0\"?>
<configuration>
  <appender name=\"ROLLING\" class=\"ch.qos.logback.core.rolling.RollingFileAppender\">
    <file>logs/$app-version.log</file>
    <rollingPolicy class=\"ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy\">
      <fileNamePattern>logs/$app-version-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
      <maxFileSize>20MB</maxFileSize>
      <maxHistory>30</maxHistory>
      <totalSizeCap>1GB</totalSizeCap>
    </rollingPolicy>
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} %-5level %logger{36} - %msg%n</pattern>
    </encoder>
  </appender>
  <root level=\"INFO\">
    <appender-ref ref=\"ROLLING\" />
  </root>
</configuration>")

(defn- make-log-config [model-name model-version]
  (s/replace logback-xml "$app-version" (str model-name "-" model-version)))

(defn- as-path [s]
  (s/replace s #"[\.\-_]" u/path-sep))

(defn- sanitize [s]
  (s/replace s "-" "_"))

(defn project-dir [model-name]
  (str cljout u/path-sep model-name u/path-sep))

(defn standalone-jar [model-name]
  (let [^File dir (File. (str (project-dir model-name) u/path-sep "target"))
        ^IOFileFilter ff (WildcardFileFilter. "*standalone*.jar")
        files (FileUtils/iterateFiles dir ff nil)]
    (when-let [rs (first (iterator-seq files))]
      (str rs))))

(defn- clj-io [model-name]
  (let [prefix (project-dir model-name)]
    [#(read-string (slurp (str prefix %)))
     (fn [file-name contents & options]
       (let [f (File. (str prefix file-name))]
         (FileUtils/createParentDirectories f)
         (with-open [w (io/writer f)]
           (cond
             (some #{:spit} options)
             (spit w contents)

             (some #{:write-each} options)
             (doseq [exp contents]
               (pprint/pprint exp w))

             :else
             (pprint/pprint contents w)))))]))

(defn- create-clj-project [model-name version]
  (let [app-name (if version (str model-name ":" version) model-name)
        cmd (str "lein new fractl-model " app-name)]
    (u/exec-in-directory cljout-file cmd)))

(defn- exec-for-model [model-name cmd]
  (u/exec-in-directory (project-dir model-name) cmd))

(defn- maybe-add-repos [proj-spec model]
  (if-let [repos (:repositories model)]
    (conj proj-spec :repositories repos)
    proj-spec))

(defn- model-version [model]
  (or (:version model) "0.0.1"))

(defn- update-project-spec [model project-spec]
  (let [deps (:clj-dependencies model)
        ver (model-version model)]
    (loop [spec project-spec, final-spec []]
      (if-let [s (first spec)]
        (if (= :dependencies s)
          (recur (rest (rest spec))
                 (conj
                  final-spec :dependencies
                  (vec (concat (second spec) deps))))
          (recur (rest spec) (conj final-spec s)))
        (seq (maybe-add-repos final-spec model))))))

(defn- find-component-declaration [component]
  (let [f (first component)]
    (when (= 'component (first f))
      f)))

(defn- write-component-clj [model-name component-name write component]
  (let [parts (s/split (str component-name) #"\.")
        compname (last parts)
        dirs (butlast parts)
        file-name
        (str
         "src" u/path-sep (sanitize model-name) u/path-sep "model" u/path-sep
         (s/join u/path-sep (concat dirs [(str compname ".clj")])))]
    (write file-name component :write-each)
    component-name))

(defn- var-name [defexp]
  (first (filter symbol? defexp)))

(defn- rewrite-in-decl [ns-name local-defs full-defs-map decl]
  (w/prewalk
   #(if (and (symbol? %)
             (some #{%} local-defs))
      (get full-defs-map % %)
      %)
   decl))

(def ^:private clj-defs #{'def 'defn 'defn-})
(def ^:private fractl-defs #{'entity 'dataflow 'event 'record})

(defn- update-local-defs [ns-name component]
  (let [local-defs (set
                    (mapv
                     #(var-name (rest %))
                     (filter #(and (seqable? %)
                                   (some #{(first %)} clj-defs))
                             component)))
        updated-defs (into {} (mapv (fn [d] [d (symbol (str ns-name "/" d))]) local-defs))
        rw (partial rewrite-in-decl ns-name local-defs updated-defs)]
    (mapv
     #(if (and (seqable? %)
               (some #{(first %)} fractl-defs))
        (rw %)
        %)
     component)))

(defn- model-refs-to-use [refs]
  (let [spec (mapv
              (fn [r]
                (let [ss (s/split (s/lower-case (name r)) #"\.")]
                  [(symbol
                    (if (= 1 (count ss))
                      (str (first ss) ".model.model")
                      (s/join "." (concat [(first ss) "model"] ss))))]))
              refs)]
    (concat spec [[(symbol "fractl.lang")]])))

(defn- merge-use-models [import-spec use-models]
  (loop [spec import-spec, result [], merged false]
    (if merged
      (concat result spec)
      (if-let [s (first spec)]
        (let [m (= :use (first s))]
          (recur (rest spec)
                 (if m
                   (conj result (concat s use-models))
                   (conj result s)) m))
        (conj result `(:use ~@use-models))))))

(defn- normalize-clj-imports [spec]
  (if (= 'quote (first spec))
    (second spec)
    spec))

(defn- copy-component [write model-name component]
  (if-let [component-decl (find-component-declaration component)]
    (let [component-name (second component-decl)
          component-spec (when (> (count component-decl) 2)
                           (nth component-decl 2))
          cns-name (symbol (s/lower-case (name component-name)))
          ns-name (symbol (str (sanitize model-name) ".model." cns-name))
          use-models (model-refs-to-use (:refer component-spec))
          clj-imports (merge-use-models
                       (normalize-clj-imports (:clj-import component-spec))
                       use-models)
          ns-decl `(~(symbol "ns") ~ns-name
                    ~@clj-imports)]
      (write-component-clj
       model-name cns-name write
       (concat
        [ns-decl]
        (update-local-defs ns-name component))))
    (u/throw-ex "no component declaration found")))

(defn- write-model-clj [write model-name component-names model]
  (let [root-ns-name (symbol (str (sanitize model-name) ".model"))
        req-comp (mapv (fn [c] [(symbol (str root-ns-name "." c))]) component-names)
        ns-decl `(~'ns ~(symbol (str root-ns-name ".model")) (:use ~@req-comp))
        model (dissoc model :clj-dependencies :repositories)]
    (write (str "src" u/path-sep (sanitize model-name) u/path-sep "model" u/path-sep "model.clj")
           [ns-decl model] :write-each)))

(defn- build-clj-project [model-name model-root model components]
  (if (create-clj-project model-name (model-version model))
    (let [[rd wr] (clj-io model-name)
          spec (update-project-spec model (rd "project.clj"))
          log-config (make-log-config model-name (model-version model))]
      (wr "project.clj" spec)
      (wr "resources/logback.xml" log-config :spit)
      (let [cmps (mapv (partial copy-component wr model-name) components)]
        (write-model-clj wr model-name cmps model)
        model-name))
    (log/error (str "failed to create clj project for " model-name))))

(declare install-model)

(defn- install-local-dependencies! [model-paths deps]
  (doseq [[model-name _ :as d] deps]
    (when-not (install-model model-paths (s/lower-case (name model-name)))
      (u/throw-ex (str "failed to install dependency " d)))))

(defn- clj-project-path [model-paths model-name]
  (first
   (su/truths
    #(let [dir (str % u/path-sep model-name)
           ^File f (File. (str dir u/path-sep "project.clj"))]
       (when (.exists f)
         dir))
    model-paths)))

(defn build-model
  ([model-paths model-name]
   (let [model-paths (or model-paths (tu/get-system-model-paths))]
     (if-let [path (clj-project-path model-paths model-name)]
       (let [^File f (File. path)]
         (FileUtils/createParentDirectories f)
         (FileUtils/copyDirectory f (File. (project-dir model-name)))
         [model-name path])
       (let [[model model-root :as result] (loader/read-model model-paths model-name)
             components (loader/read-components-from-model model model-root)
             projdir (File. (project-dir model-name))]
         (install-local-dependencies! model-paths (:local-dependencies model))
         (if (.exists projdir)
           (FileUtils/deleteDirectory projdir)
           (when-not (.exists cljout-file)
             (.mkdir cljout-file)))
         (when (build-clj-project model-name model-root model components)
           result)))))
  ([model-name]
   (build-model nil model-name)))

(defn- exec-with-build-model [cmd model-paths model-name]
  (when-let [result (build-model model-paths model-name)]
    (when (exec-for-model model-name cmd)
      result)))

(def install-model (partial exec-with-build-model "lein install"))
(def standalone-package (partial exec-with-build-model "lein uberjar" nil))

(defn deploy-library [model-name target]
  (let [cmd (case target
              :local "lein install"
              :clojars "lein deploy clojars"
              :github "lein deploy github")]
    (exec-with-build-model cmd nil model-name)))
