(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [camel-snake-kebab.core :as csk]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as s]
            [clojure.walk :as w]
            [fractl.global-state :as gs]
            [fractl.lang.tools.build.client :as cl]
            [fractl.lang.tools.loader :as loader]
            [fractl.lang.tools.util :as tu]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.util.seq :as su])
  (:import (java.io File)
           (org.apache.commons.io FileUtils)
           (org.apache.commons.io.filefilter IOFileFilter WildcardFileFilter)))

(def out-dir "out")
(def ^:private out-file (File. out-dir))

(def ^:private component-id-var "__COMPONENT_ID__")
(def ^:private model-id-var "__MODEL_ID__")

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

(defn- fetch-fractl-version [model]
  (when-let [v (:fractl-version model)]
    (if (= v "current")
      (gs/fractl-version)
      v)))

(defn- as-path [s]
  (s/replace s #"[\.\-_]" u/path-sep))

(defn- sanitize [s]
  (s/replace s "-" "_"))

(defn project-dir [model-name]
  (str out-dir u/path-sep model-name u/path-sep))

(defn standalone-jar [model-name]
  (try
    (let [^File dir (File. (str (project-dir model-name) u/path-sep "target"))
          ^IOFileFilter ff (WildcardFileFilter. "*standalone*.jar")
          files (FileUtils/iterateFiles dir ff nil)]
      (when-let [rs (first (iterator-seq files))]
        (str rs)))
    (catch Exception ex
      (log/warn (str "standalone-jar - " (.getMessage ex)))
      nil)))

(defn- make-writer [prefix]
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
          (pprint/pprint contents w))))))

(defn- client-path [model-name]
  (let [path (str (project-dir model-name) "client" u/path-sep)
        f (File. path)]
    (FileUtils/createParentDirectories f)
    (.mkdir f)
    path))

(defn- clj-io [model-name]
  (let [prefix (project-dir model-name)]
    [#(read-string (slurp (str prefix %)))
     (make-writer prefix)]))

(defn- create-clj-project [model-name version fractl-version]
  (let [app-name (if version (str model-name ":" version ":" fractl-version) model-name)
        cmd (str "lein new fractl-model " app-name)]
    (u/exec-in-directory out-file cmd)))

(defn- exec-for-model [model-name cmd]
  (let [f (partial u/exec-in-directory (project-dir model-name))]
    (if (string? cmd)
      (f cmd)
      ;; else, a vector of commands
      (every? f cmd))))

(defn- maybe-add-repos [proj-spec model]
  (if-let [repos (:repositories model)]
    (conj proj-spec :repositories repos)
    proj-spec))

(defn- model-version [model]
  (or (:version model) "0.0.1"))

(defn- fractl-deps-as-clj-deps [deps]
  (when (seq deps)
    (su/nonils
     (mapv #(if (vector? %) ; lein dependency of the form [:project-name "version"]
              %
              (when (map? %)
                (let [model-name (get % :name)
                      pkg-name (csk/->snake_case_string model-name)
                      version (get % :version)]
                  [(symbol pkg-name) (or version "0.0.1")])))
           deps))))

(defn- update-project-spec [model project-spec]
  (let [deps (vec (fractl-deps-as-clj-deps (:dependencies model)))
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
         (s/join u/path-sep (concat dirs [(str compname ".cljc")])))]
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
(def ^:private fractl-defs #{'entity 'dataflow 'event 'record 'relationship
                             'view 'attribute 'rule 'inference 'resolver})

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

(def ^:private lang-vars (vec (conj fractl-defs 'component)))

(defn- model-refs-to-use [sanitized-model-name refs]
  (let [spec (mapv
              (fn [r]
                (let [ss (s/split (s/lower-case (name r)) #"\.")
                      cid (symbol (str (s/replace (name r) "." "_") "_" component-id-var))]
                  (if (= 1 (count ss))
                    [(symbol (str sanitized-model-name ".model." (first ss))) :only [cid]]
                    [(symbol (s/join "." (concat [(first ss) "model"] ss))) :only [cid]])))
              refs)
        deps (if (= "fractl" sanitized-model-name)
               [['fractl.lang :only lang-vars]]
               [['fractl.model.model] ['fractl.lang :only lang-vars]])]
    (concat spec deps)))

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
          s-model-name (sanitize model-name)
          ns-name (symbol (str s-model-name ".model." cns-name))
          use-models (model-refs-to-use s-model-name (:refer component-spec))
          clj-imports (merge-use-models
                       (normalize-clj-imports (:clj-import component-spec))
                       use-models)
          ns-decl `(~(symbol "ns") ~ns-name
                                   ~@clj-imports)
          exps (concat
                [ns-decl]
                (update-local-defs ns-name component)
                [`(def ~(symbol (str (s/replace (name component-name) "." "_") "_" component-id-var)) ~(u/uuid-string))])]
      (if write
        (write-component-clj
         model-name cns-name write exps)
        (binding [*ns* *ns*] (doseq [exp exps] (eval exp)))))
    (u/throw-ex "no component declaration found")))

(defn- write-model-clj [write model-name component-names model]
  (let [root-ns-name (symbol (str (sanitize model-name) ".model"))
        req-comp (mapv (fn [c] [(symbol (str root-ns-name "." c)) :as (symbol (name c))]) component-names)
        ns-decl `(~'ns ~(symbol (str root-ns-name ".model")) (:require ~@req-comp))
        model (dissoc model :repositories :dependencies)]
    (write (str "src" u/path-sep (sanitize model-name) u/path-sep "model" u/path-sep "model.cljc")
           [ns-decl (if (map? model) `(fractl.lang/model ~model) model)
            `(def ~(symbol (str (s/replace (name model-name) "." "_") "_" model-id-var)) ~(u/uuid-string))]
           :write-each)))

(def ^:private config-edn "config.edn")

(defn- write-config-edn [model-root write]
  (let [src-cfg (str model-root u/path-sep config-edn)]
    (when (.exists (File. src-cfg))
      (let [cfg (u/read-config-file src-cfg)]
        (write config-edn cfg :spit)
        cfg))))

(defn- create-client-project [model-name ver fractl-ver app-config]
  (let [build-type (if (:service (:authentication app-config))
                     'prod
                     'dev)]
    (cl/build-project
     model-name ver fractl-ver
     (client-path model-name) build-type)))

(defn- build-clj-project [model-name model-root model components]
  (let [ver (model-version model)
        fractl-ver (fetch-fractl-version model)]
    (if (create-clj-project model-name ver fractl-ver)
      (let [[rd wr] (clj-io model-name)
            spec (update-project-spec model (rd "project.clj"))
            log-config (make-log-config model-name ver)]
        (wr "project.clj" spec)
        (wr "logback.xml" log-config :spit)
        (let [cmps (mapv (partial copy-component wr model-name) components)]
          (write-model-clj wr model-name cmps model)
          (create-client-project model-name ver fractl-ver (write-config-edn model-root wr))))
      (log/error (str "failed to create clj project for " model-name)))))

(defn- normalize-deps-spec [deps]
  (map (fn [elem]
         (mapv (fn [[k v]]
                 {k v}) elem)) deps))

(declare load-model install-model)

(defn- load-clj-project [model-name _ components]
  (let [f (partial copy-component nil model-name)]
    (doseq [c components]
      (f c))
    model-name))

(def ^:dynamic load-model-mode false)

(defn- get-proper-model-name [model-name spec]
  (if spec
    (let [repo-name (get-in spec [:source :repo])]
      (if repo-name
        repo-name
        (csk/->snake_case_string model-name)))
    (csk/->snake_case_string model-name)))

(defn- install-local-dependencies! [model-paths deps]
  (doseq [[model-name type version spec :as d] (normalize-deps-spec deps)]
    (when (= :fractl-model (get type :type))
      (when spec (tu/maybe-clone-model spec model-paths)))
    (if load-model-mode
      (load-model model-name)
      (when-not (install-model model-paths (get-proper-model-name model-name spec))
        (u/throw-ex (str "failed to install dependency " d))))))

(defn- clj-project-path [model-paths model-name]
  (first
   (su/truths
    #(let [dir (str % u/path-sep model-name)
           ^File f (File. (str dir u/path-sep "project.clj"))]
       (when (.exists f)
         dir))
    model-paths)))

(defn compiled-model?
  ([model-path model-name]
   (let [model-path (if (= model-path ".")
                      (System/getProperty "user.dir")
                      model-path)]
     (if (nil? model-path)
       (clj-project-path (tu/get-system-model-paths) model-name)
       (let [^File f (File. (str model-path u/path-sep "project.clj"))]
         (when (.exists f)
           model-path))))))

(defn- check-local-dependency? [deps]
  (boolean (some #(= (:type %) :fractl-model) deps)))

(defn build-model
  ([build-load-fn model-paths model-name model-info]
   (let [{model-paths :paths model :model model-root :root model-name :name}
         (loader/load-all-model-info model-paths model-name model-info)
         result [model model-root]
         fvers (fetch-fractl-version model)]
     (when-not (= fvers (gs/fractl-version))
       (u/throw-ex (str "runtime version mismatch - required " fvers ", found " (gs/fractl-version))))
     (if-let [path (clj-project-path model-paths model-name)]
       (let [^File f (File. path)]
         (FileUtils/createParentDirectories f)
         (FileUtils/copyDirectory f (File. (project-dir model-name)))
         [model-name path])
       (let [components (loader/read-components-from-model model model-root)
             projdir (File. (project-dir model-name))
             model-dependencies (:dependencies model)]
         (when (check-local-dependency? model-dependencies)
           (install-local-dependencies! model-paths model-dependencies))
         (if (.exists projdir)
           (FileUtils/deleteDirectory projdir)
           (when-not (.exists out-file)
             (.mkdir out-file)))
         (when (build-load-fn model-name model-root model components)
           [model-name result])))))
  ([model-paths model-name]
   (build-model build-clj-project model-paths model-name nil))
  ([model-name]
   (build-model nil model-name)))

(defn exec-with-build-model [cmd model-paths model-name]
  (when-let [result (build-model model-paths model-name)]
    (if cmd
      (when (exec-for-model (first result) cmd)
        (second result))
      (first result))))

(def install-model (partial exec-with-build-model "lein install"))
(def standalone-package (partial exec-with-build-model ["lein install" "lein uberjar"] nil))

(defn- maybe-copy-kernel [model-name]
  (when (= model-name "fractl")
    (FileUtils/copyDirectory
     (File. "out/fractl/src/fractl/model/fractl/kernel")
     (File. "src/fractl/model/fractl/kernel")))
  model-name)

(defn compile-model [model-name]
  (maybe-copy-kernel (exec-with-build-model nil nil model-name)))

(defn- load-script [model-root _ f]
  (when (not= :delete (:kind f))
    (try
      (loader/load-script model-root (:file f))
      (catch Exception ex
        (.printStackTrace ex)
        (log/warn (str "failed to load " (:file f) " - " (str ex)))))))

(defn- handle-load-clj-project [model-name model-root model components]
  (load-clj-project model-name model components))

(defn load-model [model-name]
  (binding [load-model-mode true]
    (build-model handle-load-clj-project nil model-name nil)))

(defn- config-file-path [model-name]
  (str (project-dir model-name) config-edn))

(defn- exec-standalone [model-name cfg]
  (when-let [jar-file (standalone-jar model-name)]
    (let [cmd (str "java -jar " jar-file " -c " cfg)]
      (println cmd)
      (u/exec-in-directory "." cmd))))

(defn run-standalone-package [model-name]
  (let [model-name (or model-name (:name (loader/load-default-model-info)))
        run #(exec-standalone model-name (config-file-path model-name))]
    (or (run) (when (standalone-package model-name) (run)))))

(defn publish-library [model-name target]
  (let [cmd (case target
              :local "lein install"
              :clojars "lein deploy clojars"
              :github "lein deploy github")]
    (exec-with-build-model cmd nil model-name)))
