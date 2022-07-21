Build and run
=============

Make sure `JAVA_HOME` is set:

E.g:

```shell
 export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64/
```

```shell
 mvn clean package
 mvn azure-functions:run
```

Deploy
======

```shell
 az login
 mvn azure-functions:deploy
```