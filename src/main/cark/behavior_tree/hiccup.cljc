(ns cark.behavior-tree.hiccup
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.hiccup.spec :as hs]))

(defn prepare [hiccup]
  (let [[tag & rest-h] hiccup
        [params rest-h] (if (map? (first rest-h))
                          [(first rest-h) (rest rest-h)]
                          [{} rest-h])]
    (into [tag params] (->> rest-h (filter identity) (map prepare)))))

(defn parse
  "Parses an hiccup behavior tree, returning the parse tree"
  ([hiccup]
   (parse hiccup ::hs/node))
  ([hiccup spec]   
   (let [hiccup (prepare hiccup)
         parsed (s/conform spec hiccup)]
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
        [node tree] ((type/get-compile-func type) tree id tag params children-ids)
        tree (tree/set-node-meta tree id {:children-ids children-ids
                                          :tag tag
                                          :params params})]
    [id (tree/set-node tree id node)]))

(defn parsed->tree [tree parsed]
  (let [[id tree] (parsed->node parsed tree)]
    (tree/set-root-node-id tree id)))




