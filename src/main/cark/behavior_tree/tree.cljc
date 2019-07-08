(ns cark.behavior-tree.tree  
  (:refer-clojure :exclude [keys])
  (:require [cark.behavior-tree.type :as type]))

(def keys "The keys found in a node tree"
  #{::root-node-id ::current-id ::tree-nodes})

(defn make []
  {::root-node-id nil
   ::current-id 0
   ::tree-nodes {}})

(defn get-next-id [tree]
  [(::current-id tree) (update tree ::current-id inc)])

(defn get-node [tree id]
  (get-in tree [::tree-nodes id]))

(defn set-node [tree id tree-node]
  (assoc-in tree [::tree-nodes id] tree-node))

(defn dissoc-node [tree id]
  (update tree ::tree-nodes dissoc id))

(defn set-root-node-id [tree id]
  (assoc tree ::root-node-id id))

(defn get-root-node-id [tree]
  (::root-node-id tree))

(defn get-root-node [tree]
  (get-node tree (get-root-node-id tree)))



(comment
  (defn compile-node [tree node children-ids]
    (let [[id tree] (get-next-id tree)
          [node tree] ((type/get-compile-func node) tree id children-ids)]
      [id (set-node tree id node)]))

  (defn compile-nodes [tree nodes]
    (reduce (fn [[ids tree] node]
              (let [[id tree] (compile-node tree node)]
                [(conj ids id) tree]))
            [[] tree] nodes))

  (defn compile-with-children [config children compile-func]
    (fn [tree id]
      (let [[children-ids tree] (compile-nodes tree children)
            compiled-node (compile-func id config children-ids)]
        [(fn with-children [ctx message arg]
           (case message
             :tick (compiled-node ctx message arg)
             :get-children-ids children-ids))
         tree])))

  )
