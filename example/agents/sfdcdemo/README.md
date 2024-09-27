# sfdc-demo

Demo that integrates Salesforce, Googlesheets and AI agents.

## Account Setup

#### Salesforce setup

1. Create a Salesforce account and login to the lightning dashboard like: https://fractlinc-dev-ed.develop.lightning.force.com/lightning/setup/SetupOneHome/home
2. Go to Apps tab in Platform Tools section and create new Connected App
    1. Check Enable OAuth box and select all OAuth scopes.
    2. Set Callback URL to any, it's not used
    3. Check Enable Client Credentials Flow
3. Edit the app's policies and set the login user as "Run as" field in Client Credentials Flow
4. Copy App's Client ID and Secret and set as env variables when running SFDC app
5. Go to Salesforce Setup -> Change Data Capture and add Lead to the list of Selected Entities

#### Google Sheets OAuth setup

1. Go to https://developers.google.com/oauthplayground/
2. In step one, select Google Sheets V4 API and authenticate using Google account
    1. Select all options (sub items)
3. Click exchange authorization code for tokens
4. Copy Client ID, Client Secret, Access Token, & Refresh Token and set them as SFDC app's env variables

## Running

### Pre-setup

Ensure that the resolver dependencies are installed
- [camel_sfdc_resolver](https://github.com/fractl-io/camel_sfdc_resolver)
- [camel_googlesheets_resolver](https://github.com/fractl-io/camel_googlesheets_resolver)

### Setup

Export the apporpriate environment variables as required by config.edn and run,

```shell
$ agent run
```

In Agentlang dev-mode set the env-var AGENTLANG_MODEL_PATHS to the root of this directory and run,

```shell
$ lein run -c /this/path/config.edn build sfdcdemo
```

Then run the generated standalone jar.

## License

Copyright Fractl, Inc
