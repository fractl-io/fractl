{:name :banklib
 :version "0.0.2"
 :agentlang-version "current"
 :components [:Banklib.Core]
 :repositories [["github" {:url "https://maven.pkg.github.com/agentlang-dev/banking"
                           ;; NOTE^ a repository named "banking" should be already
                           ;; existing under the agentlang-dev org. Otherwise, the package
                           ;; may fail to upload.

                           :username "private-token"

                           ;; GITHUB_TOKEN must point to a classic github token
                           ;; with the permissions: delete:packages, repo, write:packages
                           :password :env/GITHUB_TOKEN

                           ;; !!!NOTE!!!
                           ;; for production deployments :sign-releases
                           ;; should be true
                           :sign-releases false}]]}
