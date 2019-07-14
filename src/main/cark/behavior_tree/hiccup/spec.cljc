(ns cark.behavior-tree.hiccup.spec
  "This is the spec for the hiccup dsl of a behavior tree"
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

(s/def ::tag #(or (some? (type/get-type %))
                  (= % :<>)))

(s/def ::node (s/multi-spec node-dispatch ::node))
(s/def ::params map?)
(s/def ::child (s/spec ::node))
