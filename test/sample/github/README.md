Follow these steps to test oauth with the github resolver:

```shell
$ export FRACTL_AUTH_TEST_CLIENT_ID=client-id-of-your-github-app
$ export FRACTL_AUTH_TEST_CLIENT_SECRET=client-secret-of-your-github-app
$ fractl run -c github/config.edn run github
```
Once the server starts running, issue the following HTTP request:

```
GET http://localhost:8080/_e/Github/Issue
```
