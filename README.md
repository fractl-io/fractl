[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

# The Fractl Programming Language

Fractl unlocks the future of tri-modal development - concurrent use of 3 different ways of programming:
* Traditional coding in IDEs,
* Visual development in a no-code builder, and,
* Code generation with generative-AI.

## Fractl Loves Gen AI
As a language, Fractl is a data-oriented and declarative, with an abstraction that is closer to natural language than traditional programming languages. This makes fractl a much better fit for Gen AI-powered code generation. 
Users can rapidly build business application in Fractl from high-level specifications, without the lock-in that is common to other low-code/no-code platforms.

## Fractl is open
The Fractl language specification, its compiler and runtime are open source.

## Fractl is innovative
Fractl introduces a number of innovative concepts to programming:

1. **Graph-based Hierarchical Data Model** - compose the high-level data model of an application as hierarchical graph of business entities with relationships. Such [entities and relationships](https://docs.fractl.io/docs/concepts/data-model) are first-class constructs in Fractl.
2. **Zero-trust Programming** - tightly control operations on business entities through [declarative access-control](https://docs.fractl.io/docs/concepts/zero-trust-programming) encoded directly in the model itself.
3. **Declarative Dataflow** - express business logic as [purely-declarative patterns of data](https://docs.fractl.io/docs/concepts/declarative-dataflow).
4. **Resolvers** - use a simple, but [powerful interface](https://docs.fractl.io/docs/concepts/resolvers) to interface with external systems.
5. **Interceptors** - [extend the fractl runtime](https://docs.fractl.io/docs/concepts/interceptors) with custom capabilities.
6. **Entity-graph-Database Mapping** - take advantage of an [abstract persistence layer](https://docs.fractl.io/docs/concepts/entity-db-mapping) for fully-automated storage of entity instances.

## A Taste of Fractl

The following code snippet shows the Fractl model (i.e., program) for a micro-blogging platform. (Let's name our new micro-blogging platform as `ZipZap` :-)).

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

(entity :Feed
 {:Id {:type :UUID
       :default fractl.util/uuid-string
       :path-identity true}})

(relationship :Tweets
 {:meta {:contains [:ZipZap/User :ZipZap/Tweet]}})

(relationship :Conversation
 {:meta {:between [:ZipZap/Tweet :ZipZap/Tweet
                   :as [:OriginalTweet :Reply]
                   :one-one true]}})

(relationship :FeedTweets
 {:meta {:contains [:ZipZap/Feed :ZipZap/Tweet]}})

(relationship :UserFeed
 {:meta {:between [:ZipZap/User :ZipZap/Feed
                   :one-one true]}})
```

Save this code to a file named `zip_zap.fractl` and its ready to be run as a highly-scalable micro-blogging service!
But before you can actually run it, you need to install Fractl. The next section will help you with that.

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

The `install.sh` command will install Fractl to your home directory - e.g `/home/me/fractl-0.4.6`. You may decide to pass a custom
install location to this script - `./install.sh /home/me/programs`.

#### Testing the installation

Make sure the install location of Fractl is in the system search-path:

```shell
export PATH=$PATH:/home/me/fractl-0.4.6
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

You're all set to further explore **Fractl**. Please proceed to the official [documentation](https://docs.fractl.io/docs) pages.
