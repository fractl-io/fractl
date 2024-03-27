# auth

Session-cookie authentication sample.

## Prerequisites

You will need [Leiningen][] 2.0.0 or above installed.

[leiningen]: https://github.com/technomancy/leiningen

## Running

Run fractl:

    lein run -c config.edn example/auth/order.fractl

where `config.edn` should have the following values:

```clojure
{:service {:port 8000}
 :store {:type :h2
         :dbname "./data"}
 :logging {:syslog {}}
 :authentication {:service :okta
                  :superuser-email "superuser@acme.com"
                  :domain "dev-04676848.okta.com"
                  :auth-server "default"
                  :client-id "okta-app-client-id" ; replace this
                  :client-secret "okta-app-client-secret" ; replace this
                  :scope "openid offline_access"
                  :introspect true
                  :authorize-redirect-url "http://localhost:3000/auth/callback"
                  :client-url "http://localhost:3000/order"}}
```

To start a web server for the application, run:

    lein ring server

## Uberjar

    lein ring uberjar

## Docker - build & run

    lein ring server
    cd ../../
	lein uberjar
	docker build -t auth-demo -f example/auth/Dockerfile .
	docker run -p 8000:8000 auth-demo
