(ns cark.behavior-tree.node-defs.send-event
  "The :send-event node sends an event to the outside world. It may later be picked buy their
core/get-events function.

parameters:
- :event : a keyword or keyword returning function. The event name.
- :arg : an optional function returning the for this event"
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.event :as event]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(s/def ::event (s/or :keyword keyword?
                     :function fn?))
(s/def ::arg fn?) ; TODO : direct value ?

(defn compile-node [tree id tag params [child-id]]
  (let [[type value] (:event params)
        get-event-name (case type
                         :keyword (constantly value)
                         :function value)
        get-arg (if-let [func (:arg params)]
                  func
                  (constantly nil))]
    [(fn send-event-tick [ctx arg]
       (-> (event/add-event-out ctx (get-event-name ctx) (get-arg ctx))
           (db/set-node-status id :success)))
     tree]))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :send-event
     ::type/params-spec (s/keys :req-un [::event] :opt-un [::arg])
     ::type/compile-func compile-node})))
