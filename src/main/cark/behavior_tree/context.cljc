(ns cark.behavior-tree.context
  "The context map contain all the (live and static) data of a behavior tree.

It has a notably flat structure, where database, static tree and dynamic extent are
merged into it. So that the context also is the database and the tree, as well as other keys 
merged into these. 

This means that functions from the db and tree namespaces can be called on a context map.

Wherever a node has a function parameter, the context map is always the first (and often only) parameter passed to it."
  (:refer-clojure :exclude [keys])
  (:require [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.dynamic-extent :as de]
            [clojure.set :as set]))

(defn- log [value]
  (tap> value)
  value)

(def default-max-tick-count
  "The maximum number of node ticks that may occur during a single tree tick"
  100000)

(def specific-keys
  "Keys specific to the context map"
  #{::max-tick-count ::tick-count ::tracing ::trace-depth})

(def keys
  "Keys that can be found in a context map"
  (set/union specific-keys db/keys tree/keys de/keys))

(defn make
  "Returns a context object based on on the provided database and static tree"
  [db tree]
  (merge {::max-tick-count default-max-tick-count
          ::tick-count 0}
         db tree (de/make)))

(def base
  "A base context object"
  (make (db/make) (tree/make)))

(defn get-max-tick-count
  "Returns the maximum number of node ticks that may occur during a single tree tick. 
An error will be thrown if this number is exceeded."
  [context]
  (::max-tick-count context))

(defn set-max-tick-count
  "Sets the maximum tick count"
  [context value]
  (assoc context ::max-tick-count value))

(defn reset-tick-count
  "Resets the rick count of this context. Mostly internal use."
  [context]
  (assoc context ::tick-count 0))

(defn get-tick-count
  "Returns the current tick count since the start of the tree tick evaluation."
  [context]
  (::tick-count context))

(defn inc-tick-count
  "Increases the current tick count, possibly throwing an error is it goes over the max-tick-count threshold."
  [context]
  (if (< (get-tick-count context) (get-max-tick-count context))
    (update context ::tick-count inc)
    (throw (ex-info "Max tick count reached" {:tick-count (get-tick-count context)}))))

(defn tracing-tick
  "Ticks a node while sending some debugging information to the tap"
  [ctx node-id]
  (let [space (repeat (::trace-depth ctx) "  ")
        meta (tree/get-node-meta ctx node-id)
        tag (:tag meta)
        id (:id (:params meta))
        string (log (apply str (concat space [node-id tag (if id (str "[" id "]")) ":" (db/get-node-status ctx node-id)])))
        ctx (-> ((tree/get-node ctx node-id) (-> ctx (update ::trace-depth inc) inc-tick-count) nil)
                (update ::trace-depth dec))]
    (log (str string "->"(db/get-node-status ctx node-id)))
    ctx))

(defn tick
  "Ticks a node if it's in the :fresh or :running status, increasing the tick count."
  [ctx node-id]
  (case (db/get-node-status ctx node-id)
    (:fresh :running) (if (::tracing ctx) 
                        (tracing-tick ctx node-id)
                        ((tree/get-node ctx node-id) (inc-tick-count ctx) nil))
    (:success :failure) (if (::tracing ctx)
                          (do (log (apply str (concat (repeat (::trace-depth ctx) "  ")
                                                      ["skip:" node-id (:tag (tree/get-node-meta ctx node-id)) ":"
                                                       (db/get-node-status ctx node-id)] )))
                              ctx)
                          ctx)))

(declare cancel)

(defn set-node-status
  "Sets the status of a node, possibly cancelling it if it goes from :running to :fresh."
  [ctx node-id new-status]
  (let [old-status (db/get-node-status ctx node-id)]
    (-> (if (and (= :fresh new-status)
                 (= :running old-status))
          (cancel ctx node-id)
          ctx)
        (db/set-node-status node-id new-status))))

(defn do-nodes
  "Applies the func to each of the provided nodes, threading the context along.
func will be called with the threaded context and a node-id, and must return a possibly updated context."
  [ctx node-ids func]
  (reduce (fn [ctx node-id]
            (func ctx node-id))
          ctx node-ids))

(defn reset-nodes
  "Sets the provided nodes to the :fresh status"
  [ctx node-ids]
  (do-nodes ctx node-ids #(set-node-status %1 %2 :fresh)))

(defn get-node-children-ids
  "Returns the ids of the children nodes to the provided node-id"
  [ctx node-id]
  (:children-ids (tree/get-node-meta ctx node-id)))

(defn cancel
  "Resets the node's children, removes its data from the database, and, when the node is an on-cancel node, ticks its cancel child node."
  [ctx node-id]
  (-> (if (= :on-cancel (:tag (tree/get-node-meta ctx node-id)))
        ((tree/get-node ctx node-id) (inc-tick-count ctx) :cancel)
        ctx)
      (reset-nodes (get-node-children-ids ctx node-id))
      (db/set-node-data node-id nil)))

(defn set-tracing
  "Enabled tracing on this context, sending treacing information to the tap.
Note that this can be done at any moment."
  [ctx]
  (assoc ctx ::tracing true ::trace-depth 0))

(defn clear-tracing
  "Disables tracing on this context"
  [ctx]
  (assoc ctx ::tracing false ::trace-depth 0))

