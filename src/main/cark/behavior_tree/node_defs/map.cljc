(ns cark.behavior-tree.node-defs.map
  "The :map node loops over a sequence, re-running its child with each item of the sequence being bound to a var.
parameters:
- :seq : a sequence, or sequence returning context function
- :bind-item : the keyword name of the var to which each item will be bound

When the child succeeds :
- if there are more items in the sequence, the child will be refreshed and rerun with the next item being bound.
- if there are no more items, the :map node will succeed
When the child fails, the :map node will fail

The sequence content is determined when the :map node is :fresh"
  
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.dynamic-extent :as de]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(s/def ::seq (s/or :function fn?
                   :seq seqable?))
(s/def ::bind-item keyword?)

(defn compile-node [tree id tag params [child-id]]
  (let [[type value] (:seq params)
        get-seq (case type
                  :seq (constantly value)
                  :function value)
        bind-item (:bind-item params)]
    [(fn map-tick [ctx arg]
       (case (db/get-node-status ctx id)
         :fresh (recur (-> (db/set-node-status ctx id :running)
                           (db/set-node-data id (seq (get-seq ctx))))
                       arg)
         :running (let [[item & rest-items] (db/get-node-data ctx id)]
                    (if item
                      (let [bindings (de/get-bindings ctx)
                            ctx (-> (de/set-bindings ctx (assoc bindings bind-item item))
                                    (ctx/tick child-id)
                                    (de/set-bindings bindings))]
                        (case (db/get-node-status ctx child-id)
                          :success (recur (-> (db/set-node-data ctx id rest-items)
                                              (ctx/set-node-status child-id :fresh))
                                          arg)
                          :failure (-> (db/set-node-status ctx id :failure)
                                       (db/set-node-data id nil)
                                       (ctx/set-node-status child-id :fresh))
                          :running ctx))
                      (-> (db/set-node-status ctx id :success)
                          (db/set-node-data id nil))))))
     tree]))

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :map
     ::type/params-spec (s/keys :req-un [::seq ::bind-item])
     ::type/compile-func compile-node})))
