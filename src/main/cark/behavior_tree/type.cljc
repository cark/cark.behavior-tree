(ns cark.behavior-tree.type
  "Describes a node type"
  (:refer-clojure :exclude [keys]))

(def keys
  "The keys that make a node type up"
  #{::tag ::compile-func ::node-spec ::params-spec ::children-spec})

(defonce types (atom {}))

(defn register
  "Registers a node type, so that the hiccup compiler can find it."
  [type]
  (swap! types assoc (::tag type) type)
  type)

(defn get-type
  "Returns the node type with the provided tag name"
  [tag]
  (get @types tag))

(defn get-tag
  "Returns the tag of a node type"
  [type]
  (::tag type))

(defn get-compile-func
  "Returns the compile function of a node type. 
A compile function is passed [tree id tag params children-ids] and returns a pair vector
with first the possibly updated tree and a node's tick closure."
  [type]
  (::compile-func type))

(defn get-node-spec
  "Returns the spec for this node type's hiccup notation"
  [type]
  (::node-spec type))

(defn get-params-spec
  "Returns the spec for this node type's params hiccup notation"
  [type]
  (::params-spec type))

(defn get-children-spec
  "Returns the spec for this node type's children hiccup notation"
  [type]
  (::children-spec type))
