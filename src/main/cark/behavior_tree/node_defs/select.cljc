(ns cark.behavior-tree.node-defs.select
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]))

(defn compile-node [id tag params children-ids]
  (fn select-tick [ctx arg]
    (case (db/get-node-status ctx id)
      :fresh (recur (db/set-node-status ctx id :running) arg)
      :running (loop [ctx ctx
                      i 0]
                 (if-let [child-id (get children-ids i)]
                   (case (db/get-node-status ctx child-id)
                     :failure (recur ctx (inc i))
                     (:fresh :running) (let [ctx (ctx/tick ctx child-id)]
                                         (case (db/get-node-status ctx child-id)
                                           :failure (recur ctx (inc i))
                                           :success (-> (db/set-node-status ctx id :success)
                                                        (ctx/reset-nodes (take (inc i) children-ids)))
                                           :running ctx)))
                   (-> (db/set-node-status ctx id :failure)
                       (ctx/reset-nodes (take (inc i) children-ids))))))))

(defn register []
  (type/register
   (bn/branch
    {::type/tag :select
     ::type/compile-func compile-node})))
