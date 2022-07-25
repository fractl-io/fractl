Build fractl
============

Copy application model under `fractl/app/model`.
Compile and install the fractl dependency:

```shell
$ lein with-profile +with-model uberjar
$ cd target
$ mvn install:install-file -Dfile=./fractl-0.2.3-standalone.jar -DgroupId=fractl -DartifactId=fractl -Dversion=0.2.3 -Dpackaging=jar
```

Build and run azure function locally
====================================

Make sure `JAVA_HOME` is set:

E.g:

```shell
$ export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
```

```shell
$ mvn clean package
$ mvn azure-functions:run
```

Sample request for testing the local deployment:

```shell
$ curl --header "Content-Type: application/json" \
       --request POST \
       --data '{"Policedts.Identity/Upsert_UserGroup": {"Instance": {"Policedts.Identity/UserGroup": {"Name": "abc"}}}}' \
  http://localhost:7071/api/fractl
```

Deploy
======

```shell
$ az login
$ mvn azure-functions:deploy
```

Sample request for testing the remote deployment:

```shell
$ curl --header "Content-Type: application/json" \
       --request POST \
       --data '{"Policedts.Identity/Upsert_UserGroup": {"Instance": {"Policedts.Identity/UserGroup": {"Name": "abc"}}}}' \
  https://azure-fractl.azurewebsites.net/api/fractl
```