(defproject fractl-io/fractl "0.1.6"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.773"
                  :exclusions [com.google.code.findbugs/jsr305]]
                 [org.clojure/tools.cli "1.0.194"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/data.csv "1.0.0"]
                 [cheshire "5.9.0"]
                 ;; required for store/sfdc-metadata
                 [org.antlr/antlr-complete "3.5.2"]
                 [com.force.api/force-wsc "51.2.0"]
                 [com.force.api/force-metadata-api "51.2.0"]
                 [clj-jgit "1.0.1"]
                 [com.taoensso/timbre "5.1.0"
                  :exclusions [org.clojure/tools.reader]]
                 [seancorfield/next.jdbc "1.1.581"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [com.h2database/h2 "1.4.200"]
                 [org.mindrot/jbcrypt "0.4"]
                 [honeysql "1.0.444"]
                 [compojure "1.6.2"]
                 [http-kit "2.5.0"]
                 [cljs-http "0.1.46"]
                 [ring-cors "0.1.13"]
                 [net.cgrand/macrovich "0.2.1"]
                 [reagent "1.0.0"]
                 [cljsjs/alasql "0.6.5-0"]
                 [org.postgresql/postgresql "42.2.19"]
                 [cljc.java-time "0.1.11"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [com.cognitect/transit-cljs "0.8.264"]
                 [com.amazonaws/aws-lambda-java-core "1.2.1"]
                 [com.amazonaws/aws-lambda-java-events "3.1.0"]
                 [com.amazonaws/aws-lambda-java-log4j2 "1.2.0"]
                 [camdez/sendgrid "0.2.0"]
                 [postmark "1.4.1"]
                 [de.active-group/active-logger "b69b2f5324466648e1acd388b6aa952f101dfd99"]
                 [lambdaisland/glogi "1.0.106"]
                 [com.fzakaria/slf4j-timbre "0.3.21"]
                 [tick "0.5.0-RC1"]]

  :java-source-paths ["src/java"]
  :resource-paths ["lib/sfdc-enterprise.jar"]
  :main fractl.core
  :aot :all

  :jar-exclusions [#"(?:^|/).fractl/" #"(?:^|/).db/" #"(?:^|/).json/"]

  :uberjar-exclusions [#"(?:^|/).fractl/" #"(?:^|/).db/" #"(?:^|/).json/"]

  :plugins [[lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [s3-wagon-private "1.3.4"]
            [lein-doo "0.1.10"]
            [reifyhealth/lein-git-down "0.4.0"]]

  :middleware [lein-git-down.plugin/inject-properties]

  :git-down {de.active-group/active-logger {:coordinates kitrerp/active-logger}}

  :repositories [["public-github" {:url "git://github.com" :protocol :https}]]

  :pom-addition [:distributionManagement
                  [:repository
                    ["id" "github"]
                    ["name" "GitHub fractl.io Apache Maven Packages"]
                    ["url" "https://maven.pkg.github.com/fractl-io/fractl"]]]

  :profiles {:dev {:dependencies [[com.bhauman/rebel-readline-cljs "0.1.4" :exclusions [args4j]]
                                  [com.bhauman/figwheel-main "0.2.12"
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
