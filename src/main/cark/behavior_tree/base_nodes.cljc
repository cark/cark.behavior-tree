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


(defn leaf [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type))}
         type))

(defn decorator [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type)
                                  :children (s/& (s/+ ::hs/child)
                                                 #(= (count %) 1)))}
         type))

(defn branch [type]
  (merge {::type/node-spec (s/cat :tag ::hs/tag
                                  :params (params-spec type)
                                  :children (s/+ ::hs/child))}
         type))

