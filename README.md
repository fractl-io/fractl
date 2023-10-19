[![AppCI](https://github.com/fractl-io/fractl/actions/workflows/app.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/app.yml)
[![Fractl clj CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-clj.yml)
[![Fractl cljs CI](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml/badge.svg)](https://github.com/fractl-io/fractl/actions/workflows/fractl-cljs.yml)

# The Fractl Programming Language

Fractl is a data-oriented, declarative programming language that allows the development of business applications from high-level
models or specifications. Fractl programs (or models) can be developed using traditional code-editors or they could be 
visually-designed in the generative-AI powered Fractl Design Studio.

The Fractl language specification, its compiler and runtime are all fully open source. 

## A Taste of Fractl

The following code snippet shows a complete Fractl program that models a micro-blogging platform. (Let's name our new
micro-blogging platform `ZipZap` :-)).

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

That's it - this model can be built and deployed immediately as a highly-scalable micro-blogging service!
But before you do that, you need to install Fractl and learn to invoke its build tools. The next sections will
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
