(ns cark.behavior-tree.node-defs.on-cancel
  "The :on-cancel node has exactly two children. 
The first one is the cancel node and the second one is the payload node.
It executes its payload node in a transparent manner. If the node is cancelled, that is
when it goes from the :running state to the :fresh state, the cancel node is then executed.

Please note that the cancel node must succeed or fail in a single behavior tree tick.

This node is particularly usefull for releasing previously aquired resources, like a network socket for instance.

```clojure
[:on-cancel [:send-event {:event :release-the-socket}]
  use-socket-sub-tree]
```"
  
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.hiccup.spec :as hs]
            [clojure.spec.alpha :as s]))

(defn compile-node [tree id tag params [cancel-id payload-id]]
  [(fn on-cancel-tick [ctx arg]
     (if (= arg :cancel)
       (let [ctx (ctx/tick ctx cancel-id)]
         (if (= :running (db/get-node-status ctx cancel-id))
           (throw (ex-info "on-cancel cancel nodes must not keep running." {}))
           (ctx/set-node-status ctx cancel-id :fresh)))
       (let [ctx (ctx/tick ctx payload-id)
             payload-status (db/get-node-status ctx payload-id)]
         (case payload-status
           (:success :failure) (-> (ctx/set-node-status ctx payload-id :fresh)
                                   (db/set-node-status id payload-status))
           :running (db/set-node-status ctx id :running)))))
   tree])

(defn register []
  (type/register
   (bn/branch
    {::type/tag :on-cancel
     ::type/children-spec (s/& (s/+ ::hs/child)
                               #(= (count %) 2))
     ::type/compile-func compile-node})))

