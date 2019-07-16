(ns cark.behavior-tree.event
  "The event maps keep the events coming in from the outside world and going out to it.
We made the choice of raising errors if some incoming events are left unconsumed."
  (:refer-clojure :exclude [keys]))

(def keys #{::events-out ::events-in})

(defn make []
  {::events-in []
   ::events-out []})

;; out

(defn update-events-out
  "Updates the events for user consumptions with the provided function"
  [events func]
  (update events ::events-out func))

(defn get-events-out
  "Returns all the events from tree to user"
  [events]
  (::events-out events))

(defn add-event-out
  "Adds an event that will later be consumed by the library user"
  [events event-name event-arg]
  (update-events-out events #(conj % [event-name event-arg])))

(defn clear-events-out
  "Clears all the outbound events"
  [events]
  (update-events-out events (constantly [])))

;; in

(defn update-events-in
  "Updates the inbound events with the provided function"
  [events func]
  (update events ::events-in func))

(defn get-events-in
  "Returns all the inbound events"
  [events]
  (::events-in events))

(defn add-event-in
  "Adds an inbound event"
  [events event-name event-arg]
  (update-events-in events #(conj % [event-name event-arg])))

(defn clear-events-in
  "Clears the inbound event queue of any event."
  [events]
  (update-events-in events (constantly [])))

(defn pop-event-in
  "Returns a pair with first the matching event and next the updated events map.
The pick-event? function allows for additional filtering. It will be passed the current
event being considered, and should return true when that event should be picked, false otherwise."
  ([events event-name]
   (pop-event-in events event-name (constantly true)))
  ([events event-name pick-event?]   
   (loop [[current & rest-events] (seq (get-events-in events))
          acc []]
     (if-let [name (first current)]
       (if (and (= name event-name)
                (pick-event? (second current)))
         [current (assoc events ::events-in (into acc rest-events))]
         (recur rest-events (conj acc current)))
       [nil (assoc events ::events-in acc)]))))
