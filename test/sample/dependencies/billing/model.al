{:name :Billing
 :components [:Billing.Core]
 :agentlang-version "current"
 :dependencies (quote [{:name "Banklib"
                        :type :agentlang-model
                        :version "0.0.2"
                        :source {:type :github
                                 :org "fractl-dev"
                                 :repo "banklib"
                                 :branch "main"}}])

 ;;;; set if model is compiled to a clojure project and made available in clojars.
 ;; :dependencies (quote [{:name "Banklib"
                           :type :lein
                           :version "0.0.2"
                           :source {:type :clojars
                                    :org "fractl-dev"
                                    :package "banklib"}}])

 ;;;; set if clojure dependency is private
 ;; :repositories [["github" {:url "https://maven.pkg.github.com/agentlang-dev/banking"
 ;;                           :username "private-token"
 ;;                           :password :env/GITHUB_TOKEN}]]
 }
