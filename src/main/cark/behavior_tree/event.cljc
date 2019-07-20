(ns cark.behavior-tree.event
  "The event maps keep the events coming in from the outside world and going out to it."
  (:refer-clojure :exclude [keys]))

(defn log [value]
  (tap> value)
  value)

(def keys #{::events-out ::event-in})

(defn make []
  {::event-in nil
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

(defn get-event-in
  "Returns the inbound event"
  [events]
  (::event-in events))

(defn set-event-in
  "Sets the inbound event"
  [events event-name event-arg]
  (assoc events ::event-in [event-name event-arg]))

(defn clear-event-in
  "Clears the inbound event."
  [events]
  (assoc events ::event-in nil))

(defn pick-event
  "Returns the event-in if it exists, and the name matches. The optional pick-event? function takes the event arg and returns
true if the event should be picked."
  ([events event-name]
   (pick-event events event-name (constantly true)))
  ([events event-name pick-event?]
   (let [[name arg :as event] (get-event-in events)]
     (if (and (= name event-name)
              (pick-event? arg))
       event
       nil))))


