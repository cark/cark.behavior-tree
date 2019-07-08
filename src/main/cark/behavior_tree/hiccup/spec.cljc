(ns cark.behavior-tree.hiccup.spec
  (:require
   [clojure.spec.alpha :as s]
   [cark.behavior-tree.type :as type]))

(defn log [value]
  (tap> value)
  value)

(defmulti node-dispatch first)

(defmethod node-dispatch :default [x]
  (if-let [spec (-> (first x) type/get-type type/get-node-spec)]
    spec
    (s/cat :tag ::tag :params (s/? ::params) :children (s/* ::child))))


(s/def ::tag #(some? (type/get-type %)))

(s/def ::node (s/multi-spec node-dispatch ::node) #_ (s/cat :tag ::tag :params (s/? ::node-params) :children (s/* ::child)))
(s/def ::params map?)
(s/def ::child (s/spec ::node))
