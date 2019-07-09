(ns cark.behavior-tree.node-defs.guard-selector
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.hiccup.spec :as hs]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(defn compile-node [tree id tag params children-ids]  
  (let [[predicate-ids payload-ids] (apply mapv vector (map #(ctx/get-node-children-ids tree %) children-ids))]
    [(fn guard-selector-tick [ctx arg]
       (case (db/get-node-status ctx id)
         :fresh (recur (db/set-node-status ctx id :running) arg)
         :running (let [run-index (db/get-node-data ctx id)
                        ;; run predicates
                        [ctx new-run-index] (loop [ctx ctx
                                                   i 0]
                                              (if-let [pred-id (get predicate-ids i)]
                                                (let [ctx (ctx/tick ctx pred-id)]
                                                  (case (db/get-node-status ctx pred-id)
                                                    :running (ex-info (str "Guard predicates must succeed or fail in a"
                                                                           " single tick in a guard-selector.") {})
                                                    :failure (recur (ctx/set-node-status ctx pred-id :fresh)
                                                                    (inc i))
                                                    :success [(ctx/set-node-status ctx pred-id :fresh) i]))
                                                [ctx nil]))
                        ;; stop irrelevant payload
                        ctx (if (and run-index (not= run-index new-run-index))
                              (ctx/set-node-status ctx run-index :fresh)
                              ctx)]
                    ;; run payload
                    (if new-run-index
                      (let [pl-id (get payload-ids new-run-index)
                            ctx (ctx/tick ctx pl-id)]
                        (case (db/get-node-status ctx pl-id)
                          :success (-> (ctx/set-node-status ctx pl-id :fresh)
                                       (db/set-node-status id :success)
                                       (db/set-node-data id nil))
                          :failure (-> (ctx/set-node-status ctx pl-id :fresh)
                                       (db/set-node-status id :failure)
                                       (db/set-node-data id nil))
                          :running (db/set-node-data ctx id new-run-index)))
                      (-> (db/set-node-status ctx id :failure)
                          (db/set-node-data id nil))))))
     tree]))

(defn register []
  (type/register
   (bn/branch
    {::type/tag :guard-selector
     ::type/children-spec (s/+ (s/and ::hs/child
                                      #(= :guard (:tag %))))
     ::type/compile-func compile-node})))

