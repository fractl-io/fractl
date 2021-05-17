# fractl
![FractlCI](https://github.com/fractl-io/fractl/workflows/FractlCI/badge.svg)

Fractl Language for Domain Modeling


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