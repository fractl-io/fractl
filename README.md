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
SFDC_GIT_URL=https://url-encoded-git-username:password@repo-url
SFDC_METADATA_ROOT=repository_folder

SFDC_METADATA_ROOT should already contain an initialized git repository.
Instead of setting SFDC_GIT_URL, you may ask git to cache credentials:

   git config --global credential.helper store
   git config --global credential.helper cache
