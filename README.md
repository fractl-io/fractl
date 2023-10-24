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

The following code snippet shows the Fractl model (i.e., program) for a simple accounting application. 

```clojure
(component :Accounts.Core)

(entity :Company
 {:Name {:type :String :identity true}})

(entity :Account
 {:Name {:type :String :path-identity true}})

(relationship :Accounts
 {:meta {:contains [:Company :Account]}})

(relationship :Transaction
 {:Type {:oneof ["income" "expense"]}
  :Amount :Decimal
  :Remarks :String
  :DateCreated :Now
  :meta {:between [:Account :Account :as [:Debit :Credit]]}})

(record :BalanceReport
 {:Balance :Decimal
  :GeneratedOn :Now})

(defn- find-balance [transactions]
  (reduce (fn [b t]
            (let [op (if (= "income" (:Type t)) + -)]
              (op b (:Amount t))))
          0 transactions))

(event :GenerateReport
  {:Since :DateTime
   :Company :UUID
   :Account :UUID})

(dataflow :GenerateReport
  {:Transaction
    {:DateCreated? [:>= :GenerateReport.Since]}
    :-> [[]] ;; To do
    :as :Ts}
  {:BalanceReport
    {:Balance '(find-balance :Ts)}})
```

Save this code to a file named `accounts.fractl` and its ready to be run as a highly-scalable accounting service with RESTful APIs to perform CRUD operations and generate balance report!
But before you can actually run it, you need to install Fractl. The next section will help you with that.

## Download and Install

#### Prerequisites

1. JVM 19 or later
2. Linux, Mac OSX or a Unix emulator in Windows

Download the [Fractl CLI tool](https://raw.githubusercontent.com/fractl-io/fractl-releases/87fe3632fca9cf1e9bdd4b2655ed89fed345d6ae/fractl) and execute the model:

```shell
./fractl /path/to/accounts.fractl
```

We can create a new company using an `HTTP POST` request,

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Accounts.Core/Company": {"Name": "acme"}}' \
http://localhost:8080/_e/Accounts.Core/Company
```

To make sure the new company is persisted in the store, try the following `HTTP GET`:

```shell
curl http://localhost:8080/_e/Accounts.Core/Company/acme
```

If Fractl is working correctly, both these requests will return an `OK` status along with a `:ZipZap/User` instance.
You're all set to further explore **Fractl**. Please proceed to the official [documentation](https://docs.fractl.io/docs) pages.
