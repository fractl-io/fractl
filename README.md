[![AppCI](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml)
[![AgentLang clj CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml)
[![AgentLang cljs CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml)

# The AgentLang Programming Language
AgentLang is an open-source programming language and framework for solving complex tasks with the help of AI agents.
A typical AgentLang program involves multiple, interacting agents. An agent can be enhanced with tools, knowledge bases and
chat prompts with history. Agents can also form complex graphs of inter-relationships, which allows agents to interact in complex
ways to solve a problem. The declarative nature of AgentLang makes it really easy define all the agent execution context and their
inter-dependencies using a simple and intuitive syntax.

While most AI programming frameworks limit themselves to LLM based text-processing and generation tasks, AgentLang is designed
as a complete tool for real-world application development. As a language, AgentLang is data-oriented and declarative, with
an abstraction that is closer to natural languages than traditional programming languages. This makes AgentLang a much better
fit for Gen AI-powered code generation. Users can rapidly build business application in AgentLang from high-level
specifications - typically more than 10x faster than traditional programming languages.

## AgentLang is open
The AgentLang language specification, its compiler and runtime are open source.

The code you build in AgentLang can be run anywhere using the open source compiler and runtime, thereby avoiding the vendor
lock-in of other AI programming platforms.

## AgentLang is innovative
AgentLang introduces a number of innovative concepts to programming:

1. **First-class AI Agents** - interacting AI Agents as a language concept, developers can choose from one of the built-in agent-types, or easily add their own new types.
2. **Graph-based Hierarchical Data Model** - compose the high-level data model of an application as a hierarchical graph of business entities with relationships. Such [entities and relationships](https://docs.agentlang.io/docs/concepts/data-model) are first-class constructs in AgentLang.
3. **Zero-trust Programming** - tightly control operations on business entities through [declarative access-control](https://docs.agentlang.io/docs/concepts/zero-trust-programming) encoded directly in the model itself.
4. **Declarative Dataflow** - express business logic as [purely-declarative patterns of data](https://docs.agentlang.io/docs/concepts/declarative-dataflow).
5. **Resolvers** - use a simple, but [powerful mechanism](https://docs.agentlang.io/docs/concepts/resolvers) to interface with external systems.
6. **Interceptors** - [extend the agentlang runtime](https://docs.agentlang.io/docs/concepts/interceptors) with custom capabilities.
7. **Entity-graph-Database Mapping** - take advantage of an [abstract persistence layer](https://docs.agentlang.io/docs/concepts/entity-db-mapping) for fully-automated storage of entity instances.

## A Taste of AgentLang

The following code snippet shows a simple agent that can interact with a human user:

```clojure
(component :Chat)

(dataflow
 :InitChatAgent
 {:Agentlang.Inference.Service/Agent
  {:Name "a-chat-agent"
   :Type "chat"}
  :as :Agent}
 {:Agentlang.Inference.Service/AgentLLM
  {:Agent :Agent.Name :LLM "llm01"}}
 {:Agentlang.Inference.Service/ChatSession
  {:Messages [:q# [{:role :system :content "Ask me anything, I can even tell jokes!"}]]}
  :-> [[:Agentlang.Inference.Service/AgentChatSession :Agent]]}
 :Agent)

(inference :Session {:agent "a-chat-agent"})

(dataflow
 :Agentlang.Kernel.Lang/AppInit
 {:Agentlang.Inference.Provider/LLM
  {:Type "openai"
   :Name "llm01"
   :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
            :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
            :EmbeddingModel "text-embedding-3-small"
            :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
            :CompletionModel "gpt-3.5-turbo"}}}
 [:try {:Agentlang.Inference.Service/Agent {:Name? "a-chat-agent"}} :not-found {:InitChatAgent {}}])
```

Save this code to a file named `chat.al` and its ready to be run as a highly-scalable agent service with ready-to-use
HTTP APIs to interact with the agent. But before you can actually run it, you need to install AgentLang.
The next section will help you with that.

## Download and Install

#### Prerequisites

1. [Java SE 21](https://openjdk.org/projects/jdk/21/) or later
2. Linux, Mac OSX or a Unix emulator in Windows

Set the `OPENAI_API_KEY` environment variable to a valid API key from OpenAI:

```shell
export OPENAI_API_KEY="<openai-api-key>"
```

Download the [AgentLang CLI tool](https://raw.githubusercontent.com/agentlang-ai/agentlang/main/bin/agentlang) and run the agent:

```shell
./agent /path/to/chat.al
```

We can start a chat with the agent with the following HTTP POST:

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Chat/Session": {"UserInstruction": "tell me a joke about AI agents"}}' \
http://localhost:8080/api/Chat/Session
```

## License

Copyright 2022 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0

