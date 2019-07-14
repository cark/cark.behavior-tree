(ns cark.behavior-tree.node-defs.failure-leaf
  "The :failure-leaf is a simple node that always fails"
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]))

(defn compile-node [tree id tag params children-ids]
  [(fn failure-leaf-tick [context arg]
     (case (db/get-node-status context id)
       :fresh (db/set-node-status context id :failure)))
   tree])

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :failure-leaf
     ::type/compile-func compile-node})))


