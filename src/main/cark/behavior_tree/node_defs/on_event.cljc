(ns cark.behavior-tree.node-defs.on-event
  "The :on-event node acts in the same way as the :consume-event node, but it binds
the event argument to a var and execute its child.

Parameters:
-:event : A keyword, or keyword returning context function. The name of the event to be consumed.
-:pick? : A context function with two parameters. first the tree context, then the event argument being considered. This acts as an additional filter when considering incoming events to be consumed. The event will be consumed if this function returns a truthy value.
-:wait? : A boolean, or boolean returning context function. When true the node will stay :running until the event is consumed.
-:bind-arg : the keyword name of the var to which the event argument will be bound."
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.event :as event]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.dynamic-extent :as de]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(s/def ::event (s/or :keyword keyword?
                     :function fn?))
(s/def ::bind-arg keyword?)
(s/def ::pick? fn?)
(s/def ::wait? (s/or :boolean boolean?
                     :function fn?))

(defn compile-node [tree id tag params [child-id]]
  (let [[type value] (:event params)
        get-event-name (case type
                         :keyword (constantly value)
                         :function value)
        
        bind-arg (:bind-arg params)
        
        get-pick?-func (if-let [func (:pick? params)]
                         (fn [ctx]
                           (fn [event-arg]
                             (func ctx event-arg)))
                         (fn [ctx]
                           (constantly true)))
        
        [type value] (:wait? params)
        wait? (case type
                :boolean (constantly value)
                :function value
                nil (constantly false))]
    [(fn on-event-tick [ctx arg]
       (case (db/get-node-status ctx id)
         :fresh (recur (-> (db/set-node-status ctx id :running)
                           (db/set-node-data id [:consume nil]))
                       arg)
         :running (let [[state arg] (db/get-node-data ctx id)]
                    (case state
                      :consume (let [[event ctx] (event/pop-event-in ctx (get-event-name ctx) (get-pick?-func ctx))]
                                 (if event
                                   (recur (db/set-node-data ctx id [:run (second event)]) arg)
                                   (if (wait? ctx)
                                     ctx
                                     (-> (db/set-node-status ctx id :failure)
                                         (db/set-node-data id nil)))))
                      :run (let [saved-bindings (de/get-bindings ctx)
                                 ctx (-> (de/set-bindings ctx (assoc saved-bindings bind-arg arg))
                                         (ctx/tick child-id)
                                         (de/set-bindings saved-bindings))
                                 child-status (db/get-node-status ctx child-id)]
                             (case child-status
                               (:success :failure) (-> (ctx/set-node-status ctx child-id :fresh)
                                                       (db/set-node-data id nil)
                                                       (ctx/set-node-status id child-status))
                               :running ctx))))))
     tree]))

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :on-event
     ::type/params-spec (s/keys :req-un [::event] :opt-un [::bind-arg ::pick? ::wait?])
     ::type/compile-func compile-node})))
