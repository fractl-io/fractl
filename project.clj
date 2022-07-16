(defproject fractl-io/fractl "0.2.3"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/clojurescript "1.10.896"
                  :exclusions [com.google.code.findbugs/jsr305]]
                 [org.clojure/core.memoize "1.0.257"]
                 [org.clojure/tools.cli "1.0.206"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/data.csv "1.0.0"]
                 [cheshire "5.10.1"]
                 ;; required for store/sfdc-metadata
                 [org.antlr/antlr-complete "3.5.2"]
                 [com.force.api/force-wsc "51.2.0"]
                 [com.force.api/force-metadata-api "51.2.0"]
                 [clj-jgit "1.0.1"]
                 [com.taoensso/timbre "5.1.2"
                  :exclusions [org.clojure/tools.reader]]
                 [seancorfield/next.jdbc "1.2.659"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [com.h2database/h2 "1.4.200"]
                 [org.mindrot/jbcrypt "0.4"]
                 [honeysql "1.0.461"]
                 [compojure "1.6.2"]
                 [http-kit "2.5.3"]
                 [cljs-http "0.1.46"]
                 [ring-cors "0.1.13"]
                 [net.cgrand/macrovich "0.2.1"]
                 [cljsjs/alasql "0.6.5-0"]
                 [org.postgresql/postgresql "42.3.1"]
                 [cljc.java-time "0.1.18"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [com.cognitect/transit-cljs "0.8.269"]
                 [com.amazonaws/aws-lambda-java-core "1.2.1"]
                 [com.amazonaws/aws-lambda-java-events "3.11.0"]
                 [com.amazonaws/aws-lambda-java-log4j2 "1.5.1"]
                 [camdez/sendgrid "0.2.0"]
                 [postmark "1.4.1"]
                 [de.active-group/active-logger "b69b2f5324466648e1acd388b6aa952f101dfd99"]
                 [com.lambdaisland/glogi "1.1.144"]
                 [com.fzakaria/slf4j-timbre "0.3.21"]
                 [buddy/buddy-auth "3.0.323"]
                 [keycloak-clojure "1.28.3"]
                 [org.bitbucket.b_c/jose4j "0.7.12"]
                 [tick "0.5.0-RC1"]
                 [reagent "1.1.0"]
                 [cljsjs/react "17.0.2-0"]
                 [cljsjs/react-dom "17.0.2-0"]
                 [clj-commons/secretary "1.2.4"]
                 [babashka/babashka.curl "0.1.1"]
                 [com.fasterxml.jackson.core/jackson-databind "2.13.3"]
                 [com.cognitect.aws/api "0.8.539"]
                 [com.cognitect.aws/endpoints "1.1.12.145"]
                 [com.cognitect.aws/sns "811.2.959.0"]
                 [com.cognitect.aws/kms "814.2.1008.0"]
                 [org.clojure/data.json "2.4.0"]
                 [spec-provider "0.4.14"]]

  :java-source-paths ["src/java"]
  :resource-paths ["lib/sfdc-enterprise.jar" "lib/jbcrypt.jar" "lib/auth0-1.35.0-uber.jar"]
  :main fractl.core
  :aot :all

  :jar-exclusions [#"(?:^|/).fractl/" #"(?:^|/).db/" #"(?:^|/).json/"]

  :uberjar-exclusions [#"(?:^|/).fractl/" #"(?:^|/).db/" #"(?:^|/).json/"]

  :plugins [[lein-cljsbuild "1.1.8" :exclusions [[org.clojure/clojure]]]
            [s3-wagon-private "1.3.4"]
            [lein-doo "0.1.10"]
            [reifyhealth/lein-git-down "0.4.0"]
            [lein-ancient "1.0.0-RC3"]]

  :middleware [lein-git-down.plugin/inject-properties]

  :git-down {de.active-group/active-logger {:coordinates kitrerp/active-logger}}

  :repositories [["public-github" {:url "git://github.com" :protocol :https}]]

  :pom-addition [:distributionManagement
                  [:repository
                    ["id" "github"]
                    ["name" "GitHub fractl.io Apache Maven Packages"]
                    ["url" "https://maven.pkg.github.com/fractl-io/fractl"]]]

  :profiles {:dev {:dependencies [[com.bhauman/rebel-readline-cljs "0.1.4" :exclusions [args4j]]
                                  [com.bhauman/figwheel-main "0.2.15"
                                   :exclusions [args4j
                                                com.google.code.findbugs/jsr305
                                                org.clojure/java.classpath]]]
                   ;; setup target as a resource path
                   :resource-paths ["target" "resources" "node_modules"]

                   ;; set up an alias to invoke your figwheel build
                   :aliases  {"figwheel"  ["trampoline" "run" "-m" "figwheel.main"]
                              "fig:ui" ["trampoline" "run" "-m" "figwheel.main" "-co" "ui.cljs.edn" "-r"]
                              "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
                              "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
                              "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "fractl.test-runner"]
                              "fig:rtest"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "fractl.reagent-test-runner"]
                              "fig:ci"  ["run" "-m" "figwheel.main" "-co" "ci.cljs.edn" "-m" "fractl.test-runner"]
                              "fig:rci"  ["run" "-m" "figwheel.main" "-co" "ci.cljs.edn" "-m" "fractl.reagent-test-runner"]}
                   :clean-targets  ^{:protect false} ["target" "out"]}
             :aws-lambda {:javac-options ["-target" "11" "-source" "11" "-Xlint:-options"]
                          :resource-paths ["app"]}})
