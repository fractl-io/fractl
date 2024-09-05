(ns
 agentlang.model.model
 (:require
  [agentlang.model.agentlang.kernel.lang :as agentlang.kernel.lang]
  [agentlang.model.agentlang.kernel.identity :as agentlang.kernel.identity]
  [agentlang.model.agentlang.kernel.rbac :as agentlang.kernel.rbac]))
(agentlang.lang/model
 {:name :Agentlang,
  :agentlang-version "current",
  :components
  [:Agentlang.Kernel.Lang :Agentlang.Kernel.Identity :Agentlang.Kernel.Rbac]})
(def agentlang___MODEL_ID__ "81244b53-9638-49e9-95c5-fc8b87bc0811")
