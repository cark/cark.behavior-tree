(ns cark.behavior-tree.base-nodes
  "Some utility function with usefull defaults for the broad node types"
  (:require [cark.behavior-tree.type :as type]
            [cark.behavior-tree.hiccup.spec :as hs]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(defn params-spec
  "Returns the spec for the hiccup params map of given node type"
  [type]
  (if-let [params-spec (type/get-params-spec type)]
    params-spec
    (s/? ::hs/params)))

(defn children-spec
  "Returns the spec for the hiccup children of given node type"
  [type]
  (if-let [children-spec (type/get-children-spec type)]
    children-spec
    (s/* ::hs/child)))

(defn leaf
  "Provides usefull spec defaults for leaf node types"
  [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type))
          ::type/children-spec nil?}
         type))

(defn decorator
  "Provides usefull spec defaults for decorator node types"
  [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type)
                                  :children (children-spec
                                             (merge {::type/children-spec (s/& (s/+ ::hs/child)
                                                                               #(= (count %) 1))}
                                                    type)))}
         type))

(defn branch
  "Provides usefull spec defaults for branch node types"
  [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type)
                                  :children (children-spec
                                             (merge {::type/children-spec (s/+ ::hs/child)}
                                                    type)))}
         type))

