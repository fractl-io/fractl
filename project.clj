(defproject fractl "0.0.1"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.773"]
                 [org.clojure/tools.cli "1.0.194"]
                 [cheshire "5.9.0"]
                 [com.taoensso/timbre "5.1.0"]
                 [seancorfield/next.jdbc "1.1.581"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [com.h2database/h2 "1.4.200"]
                 [honeysql "1.0.444"]
                 [compojure "1.6.2"]
                 [http-kit "2.5.0"]
                 [cljs-http "0.1.46"]
                 [ring-cors "0.1.13"]
                 [net.cgrand/macrovich "0.2.1"]
                 [cljsjs/alasql "0.6.5-0"]
                 [cljc.java-time "0.1.11"]]

  :main fractl.core
  :aot :all

  :plugins [[lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-doo "0.1.10"]]

  :profiles {:dev {:dependencies [[com.bhauman/rebel-readline-cljs "0.1.4"]
                                  [com.bhauman/figwheel-main "0.2.12"]]
                   ;; setup target as a resource path
                   :resource-paths ["target" "resources" "node_modules"]
                   ;; set up an alias to invoke your figwheel build
                   :aliases  {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
                              "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
                              "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
                              "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "fractl.test-runner"]}
                   :clean-targets  ^{:protect false} ["target" "out"]}})
