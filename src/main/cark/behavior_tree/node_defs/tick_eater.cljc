(ns cark.behavior-tree.node-defs.tick-eater
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(s/def ::count (s/or :function fn?
                     :integer (s/and int? #(>= % 0))))

(defn compile-node [id tag params children]
  (let [[type value] (:count params)
        get-count (case type
                    :integer (constantly value)
                    :function value)]
    (fn tick-eater-tick [ctx arg]
      (case (db/get-node-status ctx id)
        :fresh (recur (-> (db/set-node-status ctx id :running)
                          (db/set-node-data id 0))
                      arg)
        :running (let [i (db/get-node-data ctx id)
                       c (get-count ctx)]
                   (if (< i c)
                     (db/update-node-data ctx id inc)
                     (-> (db/set-node-status ctx id :success)
                         (db/set-node-data id nil))))))))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :tick-eater
     ::type/params-spec (s/keys :req-un [::count])
     ::type/compile-func compile-node})))
