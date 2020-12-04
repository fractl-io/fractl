(defproject fractl "0.0.1"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.773"]
                 [cheshire "5.9.0"]
                 [com.taoensso/timbre "5.1.0"]
                 [seancorfield/next.jdbc "1.1.581"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [com.h2database/h2 "1.4.200"]
                 [honeysql "1.0.444"]
                 [net.cgrand/macrovich "0.2.1"]
                 [cljsjs/alasql "0.6.5-0"]
                 [cljc.java-time "0.1.11"]]

  :main fractl.core
  :aot :all

  :plugins [[lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-doo "0.1.10"]]

  :profiles {:dev {:dependencies [[com.bhauman/rebel-readline-cljs "0.1.4"]
                                  [thheller/shadow-cljs "2.11.7"]
																	[com.bhauman/figwheel-main "0.2.12"]]
                   ;; setup target as a resource path
                   :resource-paths ["target" "resources" "node_modules"]
                   ;; set up an alias to invoke your figwheel build
                   :aliases {"cljs:cli" ["run" "-m" "shadow.cljs.devtools.cli"]
                             "cljs:test" ["run" "-m" "shadow.cljs.devtools.cli" "compile" "test"]
                             "cljs:app" ["run" "-m" "shadow.cljs.devtools.cli" "compile" "app"]
														 "fig"       ["trampoline" "run" "-m" "figwheel.main"]
														 "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
														 "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
														 "fig:test" ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "fractl.test-runner"]}
                   :clean-targets ^{:protect false} ["target" "out"]}}

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :compiler {:main fractl.core
                           :target :bundle
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/out/index.js"
                           :output-dir "resources/public/js/compiled/out"
                           :bundle-cmd {:none ["npx" "webpack" "--mode=development"]
                                        :default ["npx" "webpack"]}
                           :source-map-timestamp true
                           ;; To console.log CLJS data-structures make sure you enable devtools in Chrome
                           ;; https://github.com/binaryage/cljs-devtools
                           :preloads [devtools.preload]}}
	      {:id "test"
		      :compiler
		      {:asset-path "target/test"
		       :main fractl.test
		       :optimizations :none
		       :output-dir "target/test"
		       :output-to "target/test.js"}
		       :source-paths ["src" "test"]}
	      ;; This next build is a compressed minified build for
               ;; production. You can build this with:
               ;; lein cljsbuild once min
               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/fractl.js"
                           :main fractl.core
                           :optimizations :advanced
                           :pretty-print false}}]})
