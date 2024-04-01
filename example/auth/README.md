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
 :store {:type :postgres
         :host #$ POSTGRES_HOST
         :dbname #$ POSTGRES_DB
         :username #$ POSTGRES_USER
         :password #$ POSTGRES_PASSWORD}
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
                  :client-url "http://localhost:3000/order"
                  :cache {:host #$ REDIS_HOST
                          :port #$ REDIS_PORT}}}
```

To start a web server for the application, run:

    lein ring server

## Uberjar

    lein ring uberjar

## Docker - build & run

    lein ring server

In a new terminal:

    cd /path/to/fractl
	lein uberjar
	docker build -t auth-demo -f example/auth/Dockerfile .
    docker run --network=host -e POSTGRES_HOST=<pg-host> -e POSTGRES_DB=<pg-dbname> -e POSTGRES_USER=<pg-user> -e POSTGRES_PASSWORD=<pg-pswd> -e REDIS_HOST=<redis-host> -e REDIS_PORT=<redis-port> auth-demo
