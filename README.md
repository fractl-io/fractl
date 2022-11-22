# fractl
[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

Fractl is a low-code, declarative programming abstraction. Our goal is to bring the best of traditional development and LCNC tools together.

![Fractl goals](https://user-images.githubusercontent.com/13515894/173900814-8486c876-b8b4-4621-a406-5f1a626b1539.png#gh-dark-mode-only)
![Fractl goals](https://user-images.githubusercontent.com/13515894/173900724-16b83469-f526-4858-9259-e6b59cba9ae2.png#gh-light-mode-only)

A Fractl model can be compiled to a production grade application. Fractl is not just a very high-level language, it's also a low-code platform. A compiled model can be deployed as a distributed service to the Fractl platform.

# Getting started with Fractl

Please refer to the [wiki](https://github.com/fractl-io/fractl/wiki)

# Installing fractl

Run the install script from this directory, as shown below:

```shell
$ ./install.sh <target_dir>
```

If the command fails, install the dependency `fractl-lein-template`

Note: For now, use an absolute path for `<target_dir>`. The issue to handle relative paths is [here](https://github.com/fractl-io/fractl/issues/692).  
This will create a the `<target_dir>/fractl-<version>` and copy the fractl uberjar, config.edn
and the `fractl` command-line tool there.
