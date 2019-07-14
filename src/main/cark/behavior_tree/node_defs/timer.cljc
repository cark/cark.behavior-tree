(ns cark.behavior-tree.node-defs.timer
  "The :timer node stays :running until a certain duration (in milliseconds) has elapsed.
parameters:
- :timer : an optional keyword or keyword returning context function. The name of this timer.
- :duration : an integer or integer returning context function. The numebr of milliseconds this node stays :running before succeeding.

This node has two modes of operation.
- named timer :
  when the :timer parameter is set, the :timer node will start counting time based on the last completed timer with the same name (or now if none were found)
- free timer :
  The duration starts counting when the :timer node passes from :fresh to :running

Although they're not always necessary, named timers are the most precise as their combined durations do not depend on the tick invokation times."
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(defn log [val]
  (tap> val)
  val)

(s/def ::duration (s/or :function fn?
                        :integer int?))
(s/def ::timer (s/or :keyword keyword?
                     :function ifn?))

(defn compile-node [tree id tag params _]
  (let [[type value] (:duration params)
        get-duration (case type                       
                       :integer (constantly value)
                       :function value)
        [type value] (:timer params)
        get-name (case type
                   :function value
                   :keyword (constantly value)
                   nil nil)]
    [(if get-name
       (fn named-timer-tick [ctx arg]
         (case (db/get-node-status ctx id)
           :fresh (recur (-> (db/set-node-data ctx id (+ (or (db/get-timer-start-time ctx (get-name ctx))
                                                             (db/get-time ctx))
                                                         (get-duration ctx)))
                             (db/set-node-status id :running))
                         arg)
           :running (let [end (db/get-node-data ctx id)
                          now (db/get-time ctx)]
                      (if (<= end now)
                        (-> (db/set-node-status ctx id :success)
                            (db/set-timer-start-time (get-name ctx) end)
                            (db/set-node-data id nil))
                        ctx))))
       (fn timer-tick [ctx arg]
         (case (db/get-node-status ctx id)
           :fresh (recur (-> (db/set-node-data ctx id (+ (db/get-time ctx) (get-duration ctx)))
                             (db/set-node-status id :running))
                         arg)
           :running (if (<= (db/get-node-data ctx id) (db/get-time ctx))
                      (-> (db/set-node-status ctx id :success)
                          (db/set-node-data id nil))
                      ctx))))
     tree]))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :timer
     ::type/params-spec (s/keys :req-un [::duration] :opt-un [::timer])
     ::type/compile-func compile-node})))
