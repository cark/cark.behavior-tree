(ns cark.behavior-tree.base-nodes
  (:require [cark.behavior-tree.type :as type]
            [cark.behavior-tree.hiccup.spec :as hs]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(defn params-spec [type]
  (if-let [params-spec (type/get-params-spec type)]
    params-spec
    (s/? ::hs/params)))

(defn children-spec [type]
  (if-let [children-spec (type/get-children-spec type)]
    children-spec
    (s/* ::hs/child)))

(defn leaf [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type))
          ::type/children-spec nil?}
         type))

(defn decorator [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type)
                                  :children (children-spec
                                             (merge {::type/children-spec (s/& (s/+ ::hs/child)
                                                                               #(= (count %) 1))}
                                                    type)))}
         type))

(defn branch [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type)
                                  :children (children-spec
                                             (merge {::type/children-spec (s/+ ::hs/child)}
                                                    type)))}
         type))

