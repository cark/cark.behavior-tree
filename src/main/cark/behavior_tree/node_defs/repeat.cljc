(ns cark.behavior-tree.node-defs.repeat
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
                     :integer (s/and int?
                                     #(>= % 0))))
(s/def ::while fn?)


(defn compile-node [tree id tag params [child-id]]
  (let [[type value] (:count params)
        get-count (case type
                    :integer (constantly value)
                    :function value
                    nil nil)
        get-while (if-let [func (:while params)]
                    func
                    (constantly true))]
    [(if get-count
       (fn counting-repeat-tick [ctx arg]
         (loop [ctx ctx
                i 0]
           (case (db/get-node-status ctx id)
             :fresh (recur (db/set-node-status ctx id :running) 0)
             :running (let [c (get-count ctx)]
                        (if (and (< i c) (get-while ctx))
                          (let [ctx (ctx/tick ctx child-id)]
                            (case (db/get-node-status ctx child-id)
                              (:success :failure) (recur (ctx/set-node-status ctx child-id :fresh) (inc i))
                              :running (-> (ctx/set-node-status ctx child-id :fresh)
                                           (db/set-node-data id i))))
                          (-> (db/set-node-status ctx id :success)
                              (db/set-node-data id nil)))))))
       (fn repeat-tick [ctx arg]
         (case (db/get-node-status ctx id)
           :fresh (recur (db/set-node-status ctx id :running) arg)
           :running (if (get-while ctx)
                      (let [ctx (ctx/tick ctx child-id)]
                        (case (db/get-node-status ctx child-id)
                          (:success :failure) (recur (ctx/set-node-status ctx child-id :fresh) arg)
                          :running (ctx/set-node-status ctx child-id :fresh)))
                      (db/set-node-status ctx id :success)))))
     tree]))

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :repeat
     ::type/params-spec (s/? (s/keys :opt-un [::count ::while]))
     ::type/compile-func compile-node})))
