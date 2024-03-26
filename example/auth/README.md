# auth

Session-cookie authentication sample.

## Prerequisites

You will need [Leiningen][] 2.0.0 or above installed.

[leiningen]: https://github.com/technomancy/leiningen

## Running

To start a web server for the application, run:

    lein ring server

## Uberjar

    lein ring uberjar

## Docker - build & run

    lein ring uberjar
    cd ../../
	lein uberjar
	docker build -t auth-demo -f example/auth/Dockerfile .
	docker run -p 8000:8000 -p 3000:3000 auth-demo
