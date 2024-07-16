(ns fractl.inference.provider.protocol)

(defprotocol AiProvider
  "The interface for all LLM providers."
  (make-embedding [p spec])
  (make-completion [p spec]))
