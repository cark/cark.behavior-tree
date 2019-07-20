(ns cark.behavior-tree.node-defs.on-event
  "Once the :on-event node becomes :running, it will be able to pick an incoming event. When it does, it will execute its child, succeeding or failing according to the child's result.

Parameters:
- :event : A keyword, or keyword returning context function. The name of the event to pick.
- :pick? : A context function with two parameters. first the tree context, then the event argument being considered. This acts as an additional filter when considering incoming events to be picked. The event will be picked if this function returns a truthy value.
- :bind-arg : the keyword name of the var to which the event argument will be bound."
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
                           (constantly true)))]
    [(fn on-event-tick [ctx arg]
       (case (db/get-node-status ctx id)
         :fresh (-> (db/set-node-status ctx id :running)
                    (db/set-node-data id [:consume nil]))
         :running (let [[state arg] (db/get-node-data ctx id)]
                    (case state
                      :consume (if-let [event (event/pick-event ctx (get-event-name ctx) (get-pick?-func ctx))]
                                 (recur (db/set-node-data ctx id [:run (second event)]) arg)
                                 ctx)
                      :run (let [ctx (de/with-binding ctx bind-arg arg #(ctx/tick % child-id))
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
     ::type/params-spec (s/keys :req-un [::event] :opt-un [::bind-arg ::pick?])
     ::type/compile-func compile-node})))
