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
              (if (seqable? id)
                [(into ids id) tree]
                [(conj ids id) tree])))
          [[] tree] children))

(defn do-node [parsed tree]
  (let [{:keys [tag params children]} parsed
        [tag-type tag-value]  tag]
    (case tag-type
      :type (let [[children-ids tree] (parsed-children->tree children tree)
                  type (type/get-type tag-value)
                  [id tree] (tree/get-next-id tree)
                  [node tree] ((type/get-compile-func type) tree id tag-value params children-ids)
                  tree (tree/set-node-meta tree id {:children-ids children-ids
                                                    :tag tag-value
                                                    :params params})]
              [id (tree/set-node tree id node)])
      :splice (parsed-children->tree children tree))))

(defn do-func-call [func params tree]
  (parsed->node (parse (apply func params)) 
                tree))

(defn parsed->node [[type parsed] tree]
  (case type
    :node (do-node parsed tree)
    :func-call (do-func-call (:func parsed) (:params parsed) tree)))

(defn parsed->tree [tree parsed]
  (let [[id tree] (parsed->node parsed tree)]
    (tree/set-root-node-id tree id)))




