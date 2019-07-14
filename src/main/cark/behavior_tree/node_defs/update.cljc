(ns cark.behavior-tree.node-defs.update
  "The :update node update the tree context and then succeeds.
parameters :
- :func : a context function that updates the tree context is being passed."
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(s/def ::func fn?)

(defn compile-node [tree id tag params [child-id]]
  (let [func (:func params)]
    [(fn update-tick [ctx arg]
       (-> (func ctx)
           (db/set-node-status id :success)))
     tree]))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :update
     ::type/params-spec (s/keys :req-un [::func])
     ::type/compile-func compile-node})))
