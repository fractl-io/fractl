# fractl
![FractlCI](https://github.com/fractl-io/fractl/workflows/FractlCI/badge.svg)

Fractl Language for Domain Modeling


Postgres configuration
======================

{:store
 {:type :postgres
  :host <pg-host-name>
  :dbname <pg-database-name>
  :username <pg-user>
  :password <pg-password>}}


SFDC environment variables
==========================
SFDC_USERNAME
SFDC_PASSWORD
SFDC_METADATA_ROOT=repository_folder
SFDC_AUTH_TOKEN
SFDC_INSTANCE_NAME (e.g d5y000001bryquae-dev-ed.my.salesforce.com)
SFDC_NAMESPACE (e.g we8)

SFDC_METADATA_ROOT should already contain an initialized git repository.
Instead of setting SFDC_GIT_URL, you may ask git to cache credentials:

   git config --global credential.helper store
   git config --global credential.helper cache

SFDC_AUTH_TOKEN can be obtained by issuing an oauth2 token request as:

   curl https://login.salesforce.com/services/oauth2/token -d "grant_type=password" -d "client_id=<app-client-id>" -d "client_secret=app-client-secret" -d "username=${SFDC_USERNAME}" -d "password=${SFDC_PASSWORD}"

