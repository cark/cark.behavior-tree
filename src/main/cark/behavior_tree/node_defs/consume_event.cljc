(ns cark.behavior-tree.node-defs.consume-event
  "The :consume-event node consumes an event if it is found in the inbound event list. 
-If the event was found, the node succeeds.
-If the event was not found, the node fails, or stays running if the :wait? parameter is true.

Parameters:
- :event : A keyword, or keyword returning context function. The name of the event to be consumed.
- :pick? : A context function with two parameters. first the tree context, then the event argument being considered. This acts as an additional filter when considering incoming events to be consumed. The event will be consumed if this function returns a truthy value.
- :wait? : A boolean, or boolean returning context function. When true the node will stay :running until the event is consumed.
- :with-arg : A context function with two parameters, first the tree context, then the consumed event argument. This must return a possibly updated tree context."
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.event :as event]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(s/def ::event (s/or :keyword keyword?
                     :function fn?))
(s/def ::with-arg fn?)
(s/def ::pick? fn?)
(s/def ::wait? (s/or :boolean boolean?
                     :function fn?))

(defn compile-node [tree id tag params [child-id]]
  (let [[type value] (:event params)
        get-event-name (case type
                         :keyword (constantly value)
                         :function value)
        
        with-arg (if-let [func (:with-arg params)]
                   func
                   (fn [ctx _] ctx))
        
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
    [(fn consume-event-tick [ctx arg]
       (let [[event ctx] (event/pop-event-in ctx (get-event-name ctx) (get-pick?-func ctx))]
         (if event
           (-> (with-arg ctx (second event))
               (db/set-node-status id :success))
           (if (wait? ctx)
             (ctx/set-node-status ctx id :running)
             (ctx/set-node-status ctx id :failure)))))
     tree]))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :consume-event
     ::type/params-spec (s/keys :req-un [::event] :opt-un [::with-arg ::pick? ::wait?])
     ::type/compile-func compile-node})))
