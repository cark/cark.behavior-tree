(ns cark.behavior-tree.node-defs.timer-init
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(defn log [val]
  (tap> val)
  val)

(s/def ::time (s/or :function fn?
                    :integer int?))
(s/def ::timer (s/or :keyword keyword?
                     :function ifn?))

(defn compile-node [tree id tag params _]
  (let [[type value] (:time params)
        get-time (case type 
                   :integer (constantly value)
                   :function value
                   nil (fn [ctx] (db/get-time ctx)))
        [type value] (:timer params)
        get-name (case type
                   :function value
                   :keyword (constantly value))]
    [(fn timer-init-tick [ctx arg]
       (-> (db/set-timer-start-time ctx (get-name ctx) (get-time ctx))
           (db/set-node-status id :success)))
     tree]))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :timer-init
     ::type/params-spec (s/keys :req-un [::timer] :opt-un [::time])
     ::type/compile-func compile-node})))


