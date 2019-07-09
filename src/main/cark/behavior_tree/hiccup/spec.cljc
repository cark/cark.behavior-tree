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


(s/def ::tag (s/or :type #(some? (type/get-type %))
                   :splice #(= % :<>)))

(s/def ::node (s/or :node (s/multi-spec node-dispatch ::node)
                    :func-call (s/cat :func fn? :params (s/* (constantly true)))))
(s/def ::params map?)
(s/def ::child (s/spec ::node))
