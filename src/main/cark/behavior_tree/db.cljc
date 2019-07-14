(ns cark.behavior-tree.db
  "The database map contains all the live data for a behavior tree.
In addition to the keys specific to this namespace, it also has the keys from the event namespace.
So that the event namespace functions may be called on a database.
The all important blackboard is a key of this database map."
  (:refer-clojure :exclude [keys])
  (:require [cark.behavior-tree.event :as event]
            [clojure.set :as set]))


(def specific-keys
  "The keys specific to this namespace"
  #{::node-status ::node-data ::blackboard ::timer-start-times ::time})

(def keys
  "All the keys of a database"
  (set/union specific-keys event/keys))

(def base-db
  "a base database"
  (merge {::node-status {}
          ::node-data {}
          ::blackboard {}
          ::time 0
          ::timer-start-times {}}
         (event/make)))

(defn make
  "Creates a new database"
  []
  base-db)

(defn extract
  "Extracts the database keys from a map. This is a O(1) operation."
  [some-map]
  (select-keys some-map keys))

(defn get-node-status
  "Returns the stored status of a node. If none is found, the :fresh status is assumed."
  [db node-id]
  (get-in db [::node-status node-id] :fresh))

(defn set-node-status
  "Sets the stored status for a node. A fresh status will dissoc this node-id from the map."
  [db node-id status]
  (if (= status :fresh)
    (update db ::node-status dissoc node-id)
    (assoc-in db [::node-status node-id] status)))

(defn get-node-data
  "Returns the node specific data. This is used internally by some node types to persist data accross ticks."
  [db node-id]
  (get-in db [::node-data node-id]))

(defn set-node-data
  "Sets the node specific data"
  [db node-id node-data]
  (if (nil? node-data)
    (update db ::node-data dissoc node-id)
    (assoc-in db [::node-data node-id] node-data)))

(defn update-node-data
  "Updates the node specific data with the specified func."
  [db node-id func]
  (update-in db [::node-data node-id] func))

(defn get-blackboard
  "Returns the blackboard"
  [db]
  (::blackboard db))

(defn set-blackboard
  "Sets the entire blackboard in one go"
  [db new-blackboard]
  (assoc db ::blackboard new-blackboard))

(defn update-blackboard
  "Updates the blackboard with the specified function"
  [db func]
  (update db ::blackboard func))

;; timers

(defn get-timer-start-time
  "Returns the next start time for a timer"
  [db timer-name]
  (get-in db [::timer-start-times timer-name]))

(defn set-timer-start-time
  "Sets the next start time for a timer"
  [db timer-name value]
  (assoc-in db [::timer-start-times timer-name] value))

(defn get-time
  "Returns the time at which this context is executing, as system milliseconds"
  [db]
  (::time db))

(defn set-time
  "Sets the context's execution time, as system milliseconds"
  [db ms]
  (assoc db ::time ms))
