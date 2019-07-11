(ns cark.behavior-tree.node-defs.bind
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.dynamic-extent :as de]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(s/def ::value (s/or :function fn?
                     :literal (constantly true)))
(s/def ::binding (s/cat :name keyword? :value ::value))
(s/def ::let (s/cat :bindings (s/+ ::binding)))

(defn make-get-bindings [bindings]
  (let [funcs (reduce (fn [funcs {:keys [name value]}]
                        (let [[tag value] value]
                          (conj funcs
                                (case tag
                                  :literal (fn [_] [name value])
                                  :function (fn [node] [name (value node)])))))
                      [] bindings)]
    (fn [node]
      (into {} (map #(% node)) funcs))))

(defn compile-node [tree id tag params [child-id]]
  (let [bindings (-> params :let :bindings)
        get-bindings (make-get-bindings bindings)]
    [(fn bind-tick [ctx arg]
       (let [bindings (get-bindings ctx)
             saved-bindings (de/get-bindings ctx)
             ctx (-> (de/set-bindings ctx (merge saved-bindings bindings))
                     (ctx/tick child-id)
                     (de/set-bindings saved-bindings))
             child-status (db/get-node-status ctx child-id)]
         (case child-status
           (:success :failure) (-> (db/set-node-status ctx id child-status)
                                   (ctx/set-node-status child-id :fresh))
           :running (-> (db/set-node-status ctx id :running)))))
     tree]))

(defn register []
  (type/register
   (bn/decorator
    {::type/tag :bind
     ::type/params-spec (s/keys :req-un [::let])
     ::type/compile-func compile-node})))

