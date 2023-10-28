(ns fractl.api
  "The main language api."
  (:require [fractl.lang :as ln]))

(def
  ^{:arglists '([spec])
    :doc "Defines a new model. `spec` is a map with the following required entries:
1. :name - the name of the model, a keyword like `:ModelName`.
2. :version - a string that identifies the version of the model, e.g \"0.0.1\".
3. :fractl-version - the version of Fractl required to run this.
4. :components - a vector with the names of all the components in the model."}
  model ln/model)

(def
  ^{:arglists '([component-name] [component-name spec])
    :doc "Defines a new component. `component-name` must be a keyword of the form
`:ModelName.ComponentName`. The `spec` is map that may contains two keys - `:clj-import` and `:refer`.
`:clj-import` must be a vector of namespace import specifications for Clojure or Java.
`:refer` must map to a vector of component names that the current component is dependent on."}
  component ln/component)

(def
  ^{:arglists '([spec])
    :doc "Defines a new attribute. `spec` is a map that specifies the type of the attribute."}
  attribute ln/attribute)

(def
  ^{:arglists '([record-name spec] [record-spec])
    :doc "Defines a new record. `record-name` must be the fully-qualified-name
of the record, as in `:ModelName.ComponentName/RecordName`.
`spec` must be a map that defines the attributes and meta-data of the record.
`record-spec` must be the map `{record-name spec}`."}
  record ln/record)

(def
  ^{:arglists '([entity-name spec] [entity-spec])
    :doc "Defines a new entity. `entity-name` must be the fully-qualified-name
of the entity, as in `:ModelName.ComponentName/EntityName`.
`spec` must be a map that defines the attributes and meta-data of the entity.
`entity-spec` must be the map `{entity-name spec}`."}
  entity ln/entity)

(def
  ^{:arglists '([event-name spec] [event-spec])
    :doc "Defines a new event. `event-name` must be the fully-qualified-name
of the event, as in `:ModelName.ComponentName/EventName`.
`spec` must be a map that defines the attributes and meta-data of the event.
`event-spec` must be the map `{event-name spec}`."}
  event ln/event)

(def
  ^{:arglists '([relationship-name spec] [relationship-spec])
    :doc "Defines a new relationship. `relationship-name` must be the fully-qualified-name
of the relationship, as in `:ModelName.ComponentName/RelationshipName`.
`spec` must be a map that defines the attributes and meta-data of the relationship.
`relationship-spec` must be the map `{relationship-name spec}`."}
  relationship ln/relationship)

(def
  ^{:arglists '([match-pattern & patterns])
    :doc "Defines a new dataflow. `match-pattern` must be either,
1. the fully-qualified-name of the event that will trigger this dataflow, or,
2. a before-after crud event-spec for an entity.
`patterns` must be a sequence of crud or command patterns."}
  dataflow ln/dataflow)
