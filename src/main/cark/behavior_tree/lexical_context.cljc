(ns cark.behavior-tree.lexical-context
  (:refer-clojure :exclude [keys]))

(def keys #{::bindings})

(defn make []
  {::bindings {}})

(defn get-var [lexical-context name]
  (get-in lexical-context [::bindings name]))

(defn set-var [lexical-context name value]
  (update lexical-context ::bindings assoc name value))

(defn update-var [lexical-context name func]
  (update-in lexical-context [::bindings name] func))

(defn set-bindings [lexical-context value]
  (assoc lexical-context ::bindings value))

(defn get-bindings [lexical-context]
  (::bindings lexical-context))
