(ns cark.behavior-tree.dynamic-extent
  "The dynamic extent stores the transient vars that were bound by
nodes like :bind"
  (:refer-clojure :exclude [keys]))

(def keys #{::bindings})

(defn make
  "Returns an empty dynamic extent"
  []
  {::bindings {}})

(defn get-var
  "Returns the value of a var"
  [dynamic-extent name]
  (get-in dynamic-extent [::bindings name]))

(defn set-var
  "Sets the value of a var"
  [dynamic-extent name value]
  (update dynamic-extent ::bindings assoc name value))

(defn update-var
  "Updates the value of a var with the provided func. Currently unused, i'm not sur this is a good idea anyways."
  [dynamic-extent name func]
  (update-in dynamic-extent [::bindings name] func))

(defn set-bindings
  "Sets the whole bindings in a single go."
  [dynamic-extent value]
  (assoc dynamic-extent ::bindings value))

(defn get-bindings
  "Returns all the bindings"
  [dynamic-extent]
  (::bindings dynamic-extent))

(defn with-binding
  "Executes func in a context where name is bound to value"
  [dynamic-extent name value func]
  (let [saved (get-bindings dynamic-extent)]
    (-> (set-var dynamic-extent name value)
        func
        (set-bindings saved))))
