(ns cark.behavior-tree.type
  (:refer-clojure :exclude [keys]))

(def keys #{::tag ::compile-func ::node-spec ::params-spec})

(defonce types (atom {}))

(defn register [type]
  (swap! types assoc (::tag type) type)
  type)

(defn get-type [tag]
  (get @types tag))

(defn get-tag [type]
  (::tag type))

(defn get-compile-func [type]
  (::compile-func type))

(defn get-node-spec [type]
  (::node-spec type))

(defn get-params-spec [type]
  (::params-spec type))
