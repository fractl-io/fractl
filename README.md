[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

# The Fractl Programming Language

Fractl is a data-oriented, declarative programming language that allows the development of business applications from high-level models or specifications. Fractl enable developers to focus on the core business requirements of their application, instead of dealing with the wiring logic and resulting accidental complexity. Fractl unlocks the future of tri-modal development - concurrent use of 3 different ways of programming:
* Traditional coding in IDEs,
* Visual development in a no-code builder, and,
* Code generation with generative-AI.

## Fractl Loves Gen AI
As a language, Fractl is data-oriented and declarative, with an abstraction that is closer to natural languages than traditional programming languages. This makes fractl a much better fit for Gen AI-powered code generation.
Users can rapidly build business application in Fractl from high-level specifications - typically more than 10x faster than traditional programming languages.

## Fractl is a bridge between Code and Low-code/No-code

Fractl, with its ultra high-level abstraction and data-oriented syntax, also acts a single abstraction for both traditional programming and visual building in Fractl Inc's Design Studio product. Code blocks in the Fractl programming language can be directly represented as visual elements in Fractl Design Studio, without any translation, and vice versa. This unique characteristic of Fractl makes it possible for developers to concurrently use multiple ways of building:

Traditional coding: The complete life-cycle of a Fractl project (design, coding, debugging and deployment) can be managed using your favorite text editor and the fractl command-line tool. Fractl models are expressed in a simple and intuitive data-oriented syntax. This means, your Fractl programs can easily integrate with existing plain-text processing tools.
No-code building: Fractl Design Studio, our no-code builder, allows Fractl models to be designed visually. It also provide an accessible browser-based interface for all aspects of project management. Design Studio is also tightly integrated with the fractl.io platform which automates all aspects of deploying and managing planet-scale SaaS applications.
Generative AI: The very-high-level, declarative nature of Fractl is a phenomenal fit for LLM-driven code generation. The Gen-AI built-into Design Studio can generate ready-to-deploy applications from pure-text descriptions of the business problem.

> Fractl is built on top of the Clojure programming language and apps built in Fractl can leverage the universe of Java and Clojure libraries.

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

1. JVM 19 or later
2. Linux, Mac OSX or a Unix emulator in Windows

Download the [Fractl CLI tool](https://raw.githubusercontent.com/fractl-io/fractl/main/bin/fractl?token=GHSAT0AAAAAACJKCQZ6U7QVI6PA6CWL7FG4ZKPU2YQ) and execute the model:

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

You're all set to further explore **Fractl**. Please proceed to the official [documentation](https://docs.fractl.io/docs) pages.

## License

Copyright 2022 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0

