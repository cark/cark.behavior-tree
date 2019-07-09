(ns cark.behavior-tree.tree  
  (:refer-clojure :exclude [keys])
  (:require [cark.behavior-tree.type :as type]))

(def keys "The keys found in a node tree"
  #{::root-node-id ::current-id ::tree-nodes ::node-meta})

(defn make []
  {::root-node-id nil
   ::current-id 0
   ::tree-nodes {}
   ::node-meta {}})

(defn get-next-id [tree]
  [(::current-id tree) (update tree ::current-id inc)])

(defn get-node [tree id]
  (get-in tree [::tree-nodes id]))

(defn set-node [tree id tree-node]
  (assoc-in tree [::tree-nodes id] tree-node))

(defn dissoc-node [tree id]
  (update tree ::tree-nodes dissoc id))

(defn set-root-node-id [tree id]
  (assoc tree ::root-node-id id))

(defn get-root-node-id [tree]
  (::root-node-id tree))

(defn get-root-node [tree]
  (get-node tree (get-root-node-id tree)))

(defn get-node-meta [tree id]
  (get-in tree [::node-meta id]))

(defn set-node-meta [tree id value]
  (assoc-in tree [::node-meta id] value))
