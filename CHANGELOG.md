# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.8] - 2024-02-09

## [0.4.7] - 2023-11-14

-   Change project-prefix for publishing in Clojars
-   Fix expression loading for cljs
-   Minor bug fixes in REPL

## [0.4.6] - 2023-11-10

-   after/before crud-events
-   New Fractl REPL support on CLI
-   GPT support on Fractl with ai endpoints
-   Support lookup of deleted instances
-   Rename of identity properties

## [0.4.5] - 2023-10-02

-   Refactor relationships syntax and internal schema
-   Optimized internal representation of instance-meta
-   Refactor RBAC logic and internal schema
-   Soft-deletion support for instances in store
-   `POST` endpoint can be used to create multiple-instances
-   `GET` endpoint can be passed a `__tree` option to fetch the relationship tree rooted on a `contains` parent
-   Data filtering using `GET` query parameters

## [0.4.0] - 2023-07-05

-   GPT integration for generating app

## [0.3.3] - 2023-06-27

-   Significant RBAC related enhancements
    -   RBAC centered around graph data model
    -   RBAC DSL
-   RESTful API changes
-   Bug fixes

## [0.3.2] - 2023-06-08

-   Replace Upsert with Create and Update
-   Relationship bug fixes

## [0.3.1] - 2023-05-31

-   Support relationships in upsert/lookup dataflows
-   Fractl Kernel is now a standalone model
-   Build integration with fx for generating frontend app
-   Whitelist support for Cognito signups
-   Auto-confirmation of users for Cognito userpools if whitelist is set to false
-   Fractl doc command to generate documentation for fractl models

## [0.3.0] - 2023-02-10

-   Graph data model with relationships
-   New Auth and RBAC infrastructure

## [0.2.3] - 2022-01-17

-   **Security fix** for log4j vulnerability
-   New auth0-user resolver
-   Auth0-user resolver to create (signup) a database user in fractl
-   Auth0-user resolver to login the user
-   Bug-fixes in compose of Auth0User entity to save the user to the database
-   Proper alias support for queries and for-each
-   Pattern evaluation can be attached to attributes via `:eval`
-   Updated libraries

## [0.2.2] - 2021-12-08

-   Dynamic query support, wildcard queries
-   Relational schema mapping for entities
-   `try` and `await` for asynchronous programming
-   The :and operator made explicit in queries
-   Meta API to return api information
-   Query filter bug fix
-   Secure passwords in the database by using bycrypt hashing
-   Bug fix to prevent full-name qualifier for special character like: %
    which is used inside `for-each`

## [0.2.1] - 2021-10-22

-   Configurable expression compiler, custom parsers can be attached to tagged :expr attribute values.
    Immediate use case is supporting declarative UI spec
-   Bug fixes in query filtering for un-indexed attributes

## [0.2.0] - 2021-10-04

-   First customer release!!!
-   Resolver changes to support nested resolvers
-   AWS Lambda support
-   Bug fixes related to timer

## [0.1.6] - 2021-09-15

-   Fix list attribute bug
-   Add comprehensive date, time and datetime support
-   Add support for Timed events and new `:Kernel/Timer` functionality
-   Add support for CSV store

## [0.1.5] - 2021-08-23

-   Add descriptive errors for different types of errors (like reference error, wrong lookup of id, attribute name mismatch)
-   Remove extra info printed on terminal when http server is running
-   Models as libraries with dependency specifications
-   Compound attributes in records
-   Direct query support in :for-each
-   Auto-generation of entities from existing DB schema

## [0.1.4] - 2021-07-13

-   Zero-trust RBAC - events execution and CRUD on entities require RBAC.
-   Trigger AppInit event on startup.
-   Data race fix in cell update.
-   Fix postgres issues.
-   `:Kernel/DateTime` is now unique-able for H2 database.
-   Updated `:Kernel/DateTime` format.
-   Show stacktrace in logs.

## [0.1.3] - 2021-06-24

-   Add salesforce/sdfc drivers(libraries)
-   Postgres support
-   Git/Email/SMS resolver
-   Support for inheritance of attributes from base record
-   Autoquoting support
-   More accurate interpretation of the numeric types
-   Enhanced Logs with support for riemann, kibana, etc with log rotations
-   New Policies for CRUD access
-   Authentication model to enforce policies
-   Policy support - RBAC and logging
-   New internal type `:Kernel/Path`.

### Added

-   Everything since the beginning!

[Unreleased]: https://github.com/fractl-io/fractl/compare/0.4.8...HEAD

[0.4.8]: https://github.com/fractl-io/fractl/compare/0.4.7...0.4.8

[0.4.7]: https://github.com/fractl-io/fractl/compare/0.4.7...0.4.7

[0.4.6]: https://github.com/fractl-io/fractl/compare/0.4.5...0.4.6

[0.4.5]: https://github.com/fractl-io/fractl/compare/0.4.0...0.4.5

[0.4.0]: https://github.com/fractl-io/fractl/compare/0.3.3...0.4.0

[0.3.3]: https://github.com/fractl-io/fractl/compare/0.3.2...0.3.3

[0.3.2]: https://github.com/fractl-io/fractl/compare/0.3.1...0.3.2

[0.3.1]: https://github.com/fractl-io/fractl/compare/0.3.0...0.3.1

[0.3.0]: https://github.com/fractl-io/fractl/compare/0.2.3...0.3.0

[0.2.3]: https://github.com/fractl-io/fractl/compare/0.2.2...0.2.3

[0.2.2]: https://github.com/fractl-io/fractl/compare/0.2.1...0.2.2

[0.2.1]: https://github.com/fractl-io/fractl/compare/0.2.0...0.2.1

[0.2.0]: https://github.com/fractl-io/fractl/compare/0.1.6...0.2.0

[0.1.6]: https://github.com/fractl-io/fractl/compare/0.1.5...0.1.6

[0.1.5]: https://github.com/fractl-io/fractl/compare/0.1.4...0.1.5

[0.1.4]: https://github.com/fractl-io/fractl/compare/0.1.3...0.1.4

[0.1.3]: https://github.com/fractl-io/fractl/compare/v0.1.2...0.1.3
