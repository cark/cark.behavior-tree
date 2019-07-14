(ns cark.behavior-tree.node-defs.guard
  "The :guard node is a special kind of branch node that can only have two children.
The first child is called the predicate node, the second child is the payload node.

It is usefull for possibly interrupting a running subtree based on the predicate result.

Each time the :guard node is executed, that is when it is :fresh or :running, the
predicate node will be re-run (refreshing it if it was in a :success or :failure state)

- If the predicate succeeds, the payload node is then executed, and its result state assigned to the :guard node.
- If the predicate fails, the payload node is not executed, or is interrupted when running, and the :guard node is set as :failure.

The predicate node must succeed or fail in a single tick."
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.hiccup.spec :as hs]
            [clojure.spec.alpha :as s]))

(defn compile-node [tree id tag params [predicate-id payload-id]]
  [(fn guard-tick [ctx arg]
     (case (db/get-node-status ctx id)        
       :fresh (recur (db/set-node-status ctx id :running) arg)       
       :running (let [ctx (ctx/tick ctx predicate-id)]
                  (case (db/get-node-status ctx predicate-id)
                    :running (throw (ex-info "Guard predicate must succeed or fail in a single tick" {}))
                    :failure (-> (ctx/set-node-status ctx predicate-id :fresh)
                                 (ctx/set-node-status payload-id :fresh)
                                 (db/set-node-status id :failure))
                    :success (let [ctx (-> (ctx/set-node-status ctx predicate-id :fresh)
                                           (ctx/tick payload-id))
                                   payload-status (db/get-node-status ctx payload-id)]
                               (case payload-status
                                 (:success :failure) (-> (ctx/set-node-status ctx payload-id :fresh)
                                                         (db/set-node-status id payload-status))
                                 :running ctx))))))
   tree])

(defn register []
  (type/register
   (bn/branch
    {::type/tag :guard
     ::type/children-spec (s/& (s/* ::hs/child)
                               #(= (count %) 2))
     ::type/compile-func compile-node})))
