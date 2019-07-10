(ns cark.behavior-tree.node-defs.trace
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]))

(defn compile-node [tree id tag params [child-id]]
  [(fn trace-tick [ctx arg]
     (case (db/get-node-status ctx id)
       :fresh (recur (db/set-node-status ctx id :running) arg)
       :running (let [ctx (-> (ctx/set-tracing ctx)
                              (ctx/tick child-id)
                              ctx/clear-tracing)]
                  (case (db/get-node-status ctx child-id)
                    :failure (-> (ctx/set-node-status ctx child-id :fresh)
                                 (db/set-node-status id :failure))
                    :success (-> (ctx/set-node-status ctx child-id :fresh)
                                 (db/set-node-status id :success))
                    :running ctx))))
   tree])

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :trace
     ::type/compile-func compile-node})))
