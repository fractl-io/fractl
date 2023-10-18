[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

# The Fractl Programming Language

Fractl is a data-oriented, declarative programming language that allows the development of business applications from high-level
models or specifications. Fractl programs (or models) can be developed using traditional code-editors or they could be 
visually-designed in the generative-AI powered Fractl Design Studio.

The Fractl language specification, its compiler and runtime are all fully open source. 

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

The `install.sh` command will install Fractl to you home directory - e.g `/home/me/fractl-0.4.5`. You may decide to pass a custom
install location to this script - `./install.sh /home/me/programs`.

#### Testing the installation

Make sure the install location of Fractl is in the system search-path:

```shell
export PATH=$PATH:/home/me/fractl-0.4.5
```

Start the Fractl REPL:

```shell
fractl repl
```

If you land in the `fractl>` prompt, Fractl is installed correctly. You're all set to further explore the language.
To learn more about Fractl, please visit the official [documentation](https://fractl.io/docs) pages.
