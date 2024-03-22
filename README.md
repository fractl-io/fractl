[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

Join us on our Discord Server: https://discord.gg/VApmKxV8

# The Fractl Programming Language

Generative AI is poised to revolutionize the entire software development landscape, with the promise of fluently writing code on behalf of programmers. However, programming is a discipline that requires precision and deep reasoning, not just fluency: minor nuances in code can lead to wildly different outcomes. As a result, AI-driven code generation needs to involve a human (developer) in the loop. Developers still need to meticulously review, refactor, and test AI-generated code (aided by AI, of course) before incorporating it into their projects. Traditional programming languages are a poor fit for this human-in-the-loop workflow: generated code is too low-level, syntactically verbose, and comprehensible only by highly-trained experts, even then requiring significant effort.

## Fractl Loves Gen AI
As a language, Fractl is data-oriented and declarative, with an abstraction that is closer to natural languages than traditional programming languages. This makes fractl a much better fit for Gen AI-powered code generation. Users can rapidly build business application in Fractl from high-level specifications - typically more than 10x faster than traditional programming languages.

## Fractl is a bridge between Code and Low-code/No-code
With the rising popularity of Low-code/No-code tools, a growing chasm has emerged between traditional programming and visual building. Traditional programming by professional developers offers immense power and flexiblity, but tends to be slow, expensive and constrained by the availability of skilled developers. In contrast, low-code/no-code tools provide speed and simplicity, but are often limited to simple use cases, lead to strong vendor lock-ins, and, as a result, have been strongly rejected by professional developers.

Fractl, with its high-level of abstraction and data-oriented syntax, bridges this chasm by being a single abstraction for both traditional programming and visual building. Code blocks in the Fractl programming language can be directly represented as visual elements in Fractl Design Studio, without any translation, and vice versa. This unique characteristic of Fractl makes it possible for developers to concurrently use multiple ways of building:

* Traditional coding in IDEs,
* Visual development in a no-code builder, and,
* Code generation with generative-AI.

## Fractl is open
The Fractl language specification, its compiler and runtime are open source.

The code you build in Fractl can be run anywhere using the open source compiler and runtime, thereby avoiding the vendor lock-in of other low-code/no-code platforms.

## Fractl is innovative
Fractl introduces a number of innovative concepts to programming:

1. **Graph-based Hierarchical Data Model** - compose the high-level data model of an application as a hierarchical graph of business entities with relationships. Such [entities and relationships](https://docs.fractl.io/docs/concepts/data-model) are first-class constructs in Fractl.
2. **Zero-trust Programming** - tightly control operations on business entities through [declarative access-control](https://docs.fractl.io/docs/concepts/zero-trust-programming) encoded directly in the model itself.
3. **Declarative Dataflow** - express business logic as [purely-declarative patterns of data](https://docs.fractl.io/docs/concepts/declarative-dataflow).
4. **Resolvers** - use a simple, but [powerful mechanism](https://docs.fractl.io/docs/concepts/resolvers) to interface with external systems.
5. **Interceptors** - [extend the fractl runtime](https://docs.fractl.io/docs/concepts/interceptors) with custom capabilities.
6. **Entity-graph-Database Mapping** - take advantage of an [abstract persistence layer](https://docs.fractl.io/docs/concepts/entity-db-mapping) for fully-automated storage of entity instances.

## A Taste of Fractl

The following code snippet shows the Fractl model (i.e., program) for a simple accounting application. 

```clojure
(component :Accounts.Core)

(entity :Company
 {:Name {:type :String :guid true}
  :rbac [{:roles ["manager"] :allow [:create]}]})

(entity :AccountHead
 {:Name {:type :String :id true}
  :rbac [{:roles ["accountant"] :allow [:create]}]})

(entity :Entry
 {:No {:type :Int :id true}
  :Type {:oneof ["income" "expense"]}
  :Amount :Decimal
  :Remarks {:type :String :optional true}
  :DateCreated :Now})

(relationship :CompanyAccounts
 {:meta {:contains [:Company :AccountHead]}})

(relationship :Transactions
 {:meta {:contains [:AccountHead :Entry]}})

(record :BalanceReport
 {:Balance :Decimal
  :GeneratedOn :Now})

(defn- find-balance [entries]
  (reduce (fn [b t]
            (let [op (if (= "income" (:Type t)) + -)]
              (op b (:Amount t))))
          0 entries))

(event :GenerateReport
 {:Since :DateTime
  :Company :String
  :AccountHead :String})

(dataflow :GenerateReport
 {:AccountHead? {}
  :-> [[:CompanyAccounts?
        {:Company {:Name? :GenerateReport.Company}}
        :GenerateReport.AccountHead]]
  :as [:A]}
 {:Entry
  {:DateCreated? [:>= :GenerateReport.Since]}
  :-> [[:Transactions? :A]]
  :as :Es}
 {:BalanceReport
  {:Balance '(find-balance :Es)}})
```

Save this code to a file named `accounts.fractl` and its ready to be run as a highly-scalable accounting service with RESTful APIs to perform CRUD operations and generate balance report!
But before you can actually run it, you need to install Fractl. The next section will help you with that.

## Download and Install

#### Prerequisites

1. [Java SE 21](https://openjdk.org/projects/jdk/21/) or later
2. Linux, Mac OSX or a Unix emulator in Windows

Download the [Fractl CLI tool](https://raw.githubusercontent.com/fractl-io/fractl/main/bin/fractl) and execute the model:

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

If Fractl is installed correctly, both these requests will return an `OK` status along with a `:Company` instance.
Listed below are a few more HTTP requests that you can try with our "accounting" application:

1. Create an account-head for the new company.

```shell
POST /_e/Accounts.Core/Company/acme/CompanyAccounts/AccountHead

{"Accounts.Core/AccountHead": {"Name": "Department01"}}
```

2. Make some transactions under the new account-head.

```shell
POST /_e/Accounts.Core/Company/acme/CompanyAccounts/AccountHead/Department01/Transactions/Entry

{"Accounts.Core/Entry":
 {"No": 1, "Type": "income",
  "Amount": 2000.0, "Remarks": "Opening balance"}}

POST /_e/Accounts.Core/Company/acme/CompanyAccounts/AccountHead/Department01/Transactions/Entry

{"Accounts.Core/Entry":
 {"No": 2, "Type": "expense",
  "Amount": 500.0, "Remarks": "Rent paid"}}
```

3. Generate the balance-report for the account-head.

```shell
POST /_e/Accounts.Core/GenerateReport

{"Accounts.Core/GenerateReport":
 {"Since": "2023-11-09T00:00:00.00",
  "Company": "acme",
  "AccountHead": "Department01"}}
```

You're all set to further explore **Fractl**. Please proceed to the official [documentation](https://docs.fractl.io/docs/intro) pages.

## License

Copyright 2022 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0

