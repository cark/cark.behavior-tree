(ns cark.behavior-tree.hiccup
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.hiccup.spec :as hs]))

(defn parse
  "Parses an hiccup behavior tree, returning the parse tree"
  ([hiccup]
   (parse hiccup ::hs/node))
  ([hiccup spec]
   (let [parsed (s/conform spec hiccup)]
     (if (= ::s/invalid parsed)
       (throw (ex-info (str "Failed to parse hiccup tree.\n"
                            (expound/expound-str spec hiccup))
                       {:reason (s/explain-data spec hiccup)
                        :hiccup hiccup}))
       parsed))))

(declare parsed->node)

(defn parsed-children->tree [children tree]
  (reduce (fn [[ids tree] parsed-node]
            (let [[id tree] (parsed->node parsed-node tree)]
              [(conj ids id) tree]))
          [[] tree] children))

(defn parsed->node [parsed tree]
  (let [{:keys [tag params children]} parsed
        [children-ids tree] (parsed-children->tree children tree)
        type (type/get-type tag)
        [id tree] (tree/get-next-id tree)
        node ((type/get-compile-func type) id tag params children-ids)
        node (fn node-dispatch [ctx message arg]
               (case message
                 :tick (node ctx arg)
                 :get-children-ids children-ids
                 :get-tag tag
                 :get-params params))]
    [id (tree/set-node tree id node)]))

(defn parsed->tree [tree parsed]
  (let [[id tree] (parsed->node parsed tree)]
    (tree/set-root-node-id tree id)))

;; (comment

;;   (declare ^{:arglists '([tree parsed-node])} parsed-node->tree)

;;   (defn parsed-children->tree [tree parsed-children]
;;     (reduce (fn [[ids tree] parsed-node]
;;               (let [[id tree] (parsed-node->tree tree parsed-node)]
;;                 [(conj ids id) tree]))
;;             [[] tree] parsed-children))

;;   (defn parsed-node->tree [tree parsed-node]
;;     (let [[children-ids tree] (parsed-children->tree tree (:children parsed-node))
;;           node-type (ntype/get-type (:tag parsed-node))]
;;       (when (nil? node-type)
;;         (throw (ex-info "Node type not found." {:type (:tag parsed-node)})))
;;       (let [[id tree] (tree/get-next-id tree)
;;             tree-node (tnode/make id node-type (:params parsed-node) children-ids)
;;             tree (tree/set-node tree id tree-node)]
;;         [id tree])))

;;   (defn parsed->tree [parsed]
;;     (let [tree (tree/make)
;;           [id tree] (parsed-node->tree tree parsed)]
;;       (tree/set-root-node-id tree id)))

;;   (defn tree-node->compiled-tree [node tree]
;;     (let [[node tree] (if-let [compile-func (ntype/get-compile-func node)]
;;                         (compile-func [node tree])
;;                         [node tree])
;;           tree (tree/set-node tree (tnode/get-id node) node)]
;;       (reduce (fn [tree child-id]
;;                 (tree-node->compiled-tree (tree/get-node tree child-id) tree))
;;               tree
;;               (tnode/get-children-ids node))))

;;   (defn tree->compiled-tree [tree]
;;     (let [root (tree/get-root-node tree)]
;;       (tree-node->compiled-tree root tree)))

;;   (defn hiccup->tree [hiccup]
;;     (-> hiccup parse parsed->tree tree->compiled-tree))
;;   )
