(ns cark.behavior-tree.tree
  "The tree map contains the static data of a behavior tree.
That's the node themselves with their tick functions and parameters"
  (:refer-clojure :exclude [keys])
  (:require [cark.behavior-tree.type :as type]))

(def keys "The keys found in a node tree"
  #{::root-node-id ::current-id ::tree-nodes ::node-meta})

(defn make
  "Returns an empty tree"
  []
  {::root-node-id nil
   ::current-id 0
   ::tree-nodes {}
   ::node-meta {}})

(defn get-next-id
  "Returns a vector pair with the next id and an updated tree"
  [tree]
  [(::current-id tree) (update tree ::current-id inc)])

(defn get-node
  "Returns a node by id. A node is just a closure that can be called with a context."
  [tree id]
  (get-in tree [::tree-nodes id]))

(defn set-node
  "Sets the node at the provided id"
  [tree id tree-node]
  (assoc-in tree [::tree-nodes id] tree-node))

(defn dissoc-node
  "Removes a node from the tree"
  [tree id]
  (update tree ::tree-nodes dissoc id))

(defn set-root-node-id
  "Set the roo node id"
  [tree id]
  (assoc tree ::root-node-id id))

(defn get-root-node-id
  "Returns the id of the root node"
  [tree]
  (::root-node-id tree))

(defn get-root-node
  "Returns the root node"
  [tree]
  (get-node tree (get-root-node-id tree)))

(defn get-node-meta
  "Returns the node meta-data, that includes tag, params and children-ids"
  [tree id]
  (get-in tree [::node-meta id]))

(defn set-node-meta
  "Sets the node meta-data"
  [tree id value]
  (assoc-in tree [::node-meta id] value))
