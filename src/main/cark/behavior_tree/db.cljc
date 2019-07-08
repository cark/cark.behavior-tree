(ns cark.behavior-tree.db
  (:refer-clojure :exclude [keys])
  (:require #_[cark.behavior-tree.event :as event]
            #_[cark.behavior-tree.lexical-context :as lc]
            [clojure.set :as set]))


(def specific-keys "The keys found in a database"
  #{::node-status ::node-data ::blackboard ::timer-start-times})

(def keys
  (set/union specific-keys #_event/keys #_lc/keys))

(def base-db
  (merge {::node-status {}
          ::node-data {}
          ::blackboard {}
          ::timer-start-times {}}
         ;;(event/make)
         ;;(lc/make)
         ))

(defn make []
  base-db)

(defn extract [some-map]
  (select-keys some-map keys))

(defn get-node-status [db node-id]
  (get-in db [::node-status node-id] :fresh))

(defn set-node-status [db node-id status]
  (if (= status :fresh)
    (update db ::node-status dissoc node-id)
    (assoc-in db [::node-status node-id] status)))

(defn get-node-data [db node-id]
  (get-in db [::node-data node-id]))

(defn set-node-data [db node-id node-data]
  (if (nil? node-data)
    (update db ::node-data dissoc node-id)
    (assoc-in db [::node-data node-id] node-data)))

(defn update-node-data [db node-id func]
  (update-in db [::node-data node-id] func))

(defn get-blackboard [db]
  (::blackboard db))

(defn set-blackboard [db new-blackboard]
  (assoc db ::blackboard new-blackboard))

(defn update-blackboard [db func]
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
