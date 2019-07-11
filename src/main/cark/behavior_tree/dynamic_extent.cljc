(ns cark.behavior-tree.dynamic-extent
  (:refer-clojure :exclude [keys]))

(def keys #{::bindings})

(defn make []
  {::bindings {}})

(defn get-var [dynamic-extent name]
  (get-in dynamic-extent [::bindings name]))

(defn set-var [dynamic-extent name value]
  (update dynamic-extent ::bindings assoc name value))

(defn update-var [dynamic-extent name func]
  (update-in dynamic-extent [::bindings name] func))

(defn set-bindings [dynamic-extent value]
  (assoc dynamic-extent ::bindings value))

(defn get-bindings [dynamic-extent]
  (::bindings dynamic-extent))
