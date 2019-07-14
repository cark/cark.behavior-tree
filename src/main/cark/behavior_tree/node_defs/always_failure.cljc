(ns cark.behavior-tree.node-defs.always-failure
  "The :always-failure node has no specific parameter.
It ticks its child and responds to the result in this way :
- Child is :running -> always-failure is :running
- Child is :success -> always-failure is :failure
- Child is :failure -> alway-failure is :failure"
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]))

(defn compile-node [tree id tag params [child-id]]
  [(fn always-failure-tick [ctx arg]
     (let [ctx (ctx/tick ctx child-id)]
       (case (db/get-node-status ctx child-id)
         (:success :failure) (-> (ctx/set-node-status ctx child-id :fresh)
                                 (db/set-node-status id :failure))
         :running (db/set-node-status ctx id :running))))
   tree])

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :always-failure
     ::type/compile-func compile-node})))
