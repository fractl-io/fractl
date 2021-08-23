# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.5] - 2021-08-23

- Add descriptive errors for different types of errors (like reference error, wrong lookup of id, attribute name mismatch)
- Remove extra info printed on terminal when http server is running

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

[Unreleased]: https://github.com/fractl-io/fractl/compare/0.1.5...HEAD

[0.1.5]: https://github.com/fractl-io/fractl/compare/0.1.4...0.1.5

[0.1.4]: https://github.com/fractl-io/fractl/compare/0.1.3...0.1.4

[0.1.3]: https://github.com/fractl-io/fractl/compare/v0.1.2...0.1.3
