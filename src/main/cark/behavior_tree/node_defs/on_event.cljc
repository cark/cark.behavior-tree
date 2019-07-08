(ns cark.behavior-tree.node-defs.on-event
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.event :as event]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.lexical-context :as lc]
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

(defn compile-node [id tag params [child-id]]
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
    (fn on-event-tick [ctx arg]
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
                     :run (let [saved-bindings (lc/get-bindings ctx)
                                ctx (-> (lc/set-bindings ctx (assoc saved-bindings bind-arg arg))
                                        (ctx/tick child-id)
                                        (lc/set-bindings saved-bindings))
                                child-status (db/get-node-status ctx child-id)]
                            (case child-status
                              (:success :failure) (-> (ctx/set-node-status ctx child-id :fresh)
                                                      (db/set-node-data id nil)
                                                      (ctx/set-node-status id child-status))
                              :running ctx))))))))

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :on-event
     ::type/params-spec (s/keys :req-un [::event] :opt-un [::bind-arg ::pick? ::wait?])
     ::type/compile-func compile-node})))
