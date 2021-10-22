# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/fractl-io/fractl/compare/0.2.1...HEAD

[0.2.1]: https://github.com/fractl-io/fractl/compare/0.2.0...0.2.1

[0.2.0]: https://github.com/fractl-io/fractl/compare/0.1.6...0.2.0

[0.1.6]: https://github.com/fractl-io/fractl/compare/0.1.5...0.1.6

[0.1.5]: https://github.com/fractl-io/fractl/compare/0.1.4...0.1.5

[0.1.4]: https://github.com/fractl-io/fractl/compare/0.1.3...0.1.4

[0.1.3]: https://github.com/fractl-io/fractl/compare/v0.1.2...0.1.3
