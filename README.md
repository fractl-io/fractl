[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

# The Fractl Programming Language

Fractl is a data-oriented, declarative programming language for developing business applications from high-level specifications.
Fractl programs (also known as `models`) can be developed using traditional code-editors or they could be visually-designed in the generative-AI powered Fractl Design Studio.

The Fractl language specification, its compiler and runtime are all open source.

## What Makes Fractl Special

Fractl introduces a number of innovative concepts to programming:

1. **Graph-based Hierarchical Data Model** - decompose the high-level design of an application into graph-like or hierarchical relationships between business entities. Such [relationships](https://fractl.io/docs/concepts/data-model) are first-class constructs in Fractl.
2. **Zero-trust Programming** - tightly control operations on business entities through [declarative access-control-rules](https://fractl.io/docs/concepts/zero-trust-programming) encoded directly in the program.
3. **Declarative Dataflow** - express business logic as [purely-declarative patterns of data](https://fractl.io/docs/concepts/declarative-dataflow).
4. **Resolvers** - use a powerful [functional interface](https://fractl.io/docs/concepts/resolvers) to control how dataflow-patterns are interpreted.
5. **Interceptors** - [run custom Clojure code](https://fractl.io/docs/concepts/interceptors) before and after a pattern evaluation happens.
6. **Entity-graph-Database Mapping** - take advantage of an [abstract persistence layer](https://fractl.io/docs/concepts/entity-db-mapping) for fully-automated storage of entity instances.
7. **Generative-AI** - allow the built-in Gen-AI to transform a pure-text description of the problem to an immediately executable Fractl application, all in the matter of a few seconds.

## A Taste of Fractl

The following code snippet shows the Fractl model for a micro-blogging platform. (Let's name our new micro-blogging platform `ZipZap` :-)).

```clojure
(component :ZipZap)

(entity :User
 {:Email {:type :Email
          :identity true}
  :FirstName :String
  :LastName :String})

(entity :Tweet
 {:Id {:type :UUID
       :default fractl.util/uuid-string
       :path-identity true}
  :Created :Now
  :Body :String})

(relationship :Feed
 {:meta {:contains [:ZipZap/User :ZipZap/Tweet]}})

(relationship :Conversation
 {:meta {:between [:ZipZap/Tweet :ZipZap/Tweet
                   :as [:OriginalTweet :Reply]
                   :one-one true]}})
```

Save this code to a file named `zip_zap.fractl` and its ready to be run as a highly-scalable micro-blogging service!
But before you can actually run it, you need to install Fractl and learn to invoke its build tools. The next sections will
help you with that.

## Download and Install

#### Prerequisites

1. JDK 19 or later
2. The [Leiningen](https://leiningen.org) build automation tool

Checkout the Fractl source code from our canonical Git repository [github.com/fractl-io/fractl](https://github.com/fractl-io/fractl)
and run the following shell commands:

```shell
cd fractl
./install.sh
```

The `install.sh` command will install Fractl to your home directory - e.g `/home/me/fractl-0.4.5`. You may decide to pass a custom
install location to this script - `./install.sh /home/me/programs`.

#### Testing the installation

Make sure the install location of Fractl is in the system search-path:

```shell
export PATH=$PATH:/home/me/fractl-0.4.5
```

Start the ZipZap service using the following command:

```shell
fractl zip_zap.fractl
```

We can create a new ZipZap user with an `HTTP POST` request,

```shell
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"ZipZap/User": {"Email": "jane@zipzap.com", "FirstName": "Jane", "LastName": "J"}}' \
http://localhost:8080/_e/ZipZap/User
```

To make sure the new user is persisted in the store, try the following `HTTP GET`:

```shell
curl http://localhost:8080/_e/ZipZap/User/jane@zipzap.com
```

If Fractl is installed correctly, both these requests will return an `OK` status along with a `:ZipZap/User` instance.

You're all set to further explore the language. Please proceed to the official [documentation](https://fractl.io/docs) pages.
