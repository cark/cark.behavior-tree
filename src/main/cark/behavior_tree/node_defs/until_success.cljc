(ns cark.behavior-tree.node-defs.until-success
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(defn compile-node [tree id tag params [child-id]]
  [(fn until-success-tick [ctx arg]
     (let [ctx (ctx/tick ctx child-id)
           child-status (db/get-node-status ctx child-id)]
       (case child-status
         :success (-> (db/set-node-status ctx id :success)
                      (ctx/set-node-status child-id :fresh))
         :failure (recur (ctx/set-node-status child-id :fresh) arg)
         :running (db/set-node-status ctx id :running))))
   tree])

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :until-success
     ::type/compile-func compile-node})))
