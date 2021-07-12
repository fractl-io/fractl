# fractl
[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

Fractl Language for Domain Modeling

## Logging setup:

Fractl repo provides 4 types of log appenders only one of which is switched on by default.

1. `print` to console/terminal
2. Write to a file and rotate it. (Default)
3. Stream to `reimann`
4. `filebeat->logstash->elasticsearch<-kibana` setup.

Note: In production logging will always be (2) with (3) or (4), or some variant of it.

To get how to configure and try other setups configure `fractl.util.logger` if you want to
have a direct stream setup to open ports of `riemann` or `logstash` else, you can use the provided
`docker-compose.yml` and provided conf files of `logstash`, `filebeat`, `riemann` and such.

### Running Docker compose for ELK(filebeat) and Riemann similar to production system:
1. Install docker on your machine. (Obviously)
2. Install docker-cli or docker-compose.
```shell
docker-compose up
OR
docker-compose up -d # to run it in detached mode in background
```


## Postgres configuration
```clojure
{:store
 {:type :postgres
  :host <pg-host-name>
  :dbname <pg-database-name>
  :username <pg-user>
  :password <pg-password>}}

```

## SFDC environment variables
```dotenv
SFDC_USERNAME
SFDC_PASSWORD
SFDC_METADATA_ROOT=repository_folder
SFDC_AUTH_TOKEN
SFDC_INSTANCE_NAME (e.g d5y000001bryquae-dev-ed.my.salesforce.com)
SFDC_NAMESPACE (e.g we8)
SSH_KEY=(e.g. "id_ventur8") 
;; OR
GIT_USERNAME=(e.g. "Ranga")
GIT_PASSWORD=(e.g. "CreateSomethingNew12")
```

`SFDC_AUTH_TOKEN` can be obtained by issuing an oauth2 token request as:

```shell
curl https://login.salesforce.com/services/oauth2/token -d "grant_type=password" -d "client_id=<app-client-id>" -d "client_secret=app-client-secret" -d "username=${SFDC_USERNAME}" -d "password=${SFDC_PASSWORD}"
```

### Git wrapper:
`SFDC_METADATA_ROOT` should already contain an initialized git repository.
Either `SSH_KEY` with the key file name or `GIT_USERNAME` and `GIT_PASSWORD` must be provided.
Use `SSH_KEY` if you have cloned the repo using SSH as later might not work for remote ssh URL.

### Limitation of Git wrapper:
1. Password based SSH file is problematic. (To get around this use `GIT_USERNAME` and `GIT_PASSWORD` for HTTPS auth.)
2. `commit.gpgSign` should be false in the metadata repo. As, `gpgSigning` is not supported for now.

## Email Resolver

```dotenv
EMAIL_API_KEY=<some-key> ;;API key from either SendGrid or Postmark
EMAIL_SENDER=<registered email id> ;;Registered email for either SendGrid or Postmark
```
Email resolver requires following keys:
```clojure
{:Backend "SendGrid" or "Postmark"
 :Receiver "someemail@ventur8.io"
 :Subject "This is demo."
 :Text "Hi."}
```

## Sms Resolver
```dotenv
TWILIO_SID=<sid from twilio>
TWILIO_AUTH_TOKEN=<token from Twilio>
TWILIO_PHONE_NUMBER=<phone number provided from twilio>
```

Sms resolver requires following keys:
```clojure
{:To "Phone number (e.g. +977XXXXXXXXXX"
 :Body "Text message (e.g. Hi. How are you doing?)"}
```

## Using docker for postgres
In macos homebrew based postgres provides false positive so, rather than having deal with
that, there is alternative to use docker for running postgres as, Dockerfile is provided.

#### Build image:

```shell
docker build -t postgresql .
```

#### Run image exposing ports:

```shell
docker run -p 127.0.0.1:5432:5432/tcp --rm -P --name pg_test postgresql
```