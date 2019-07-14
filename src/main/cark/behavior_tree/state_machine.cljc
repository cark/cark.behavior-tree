(ns cark.behavior-tree.state-machine
  "Provides a state-machine implementation as a client library to the
behavior tree library. Some book keeping data will thus be stored in the blackboard.

This merely creates a hiccup tree, that will then need to be compiled and used like a regular
behavior tree."
  (:require [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.hiccup :as hiccup]
            [cark.behavior-tree.event :as event]))

(defn log [value]
  (tap> value)
  value)

(defn- get-path [node & rest]
  (into (bt/get-var node ::path) rest))

(defn- in-state? [name]
  [:predicate {:func #(= (bt/bb-get-in % (get-path %)) name)}])

(defn make
  "Creates the hiccup for a state machine. Its data will be stored in the black board at 
the specified path. Upon entering this node, the initial state will directly be activated."
  [path initial-state & states]
  [:bind {:let [::path (vec path)]}
   [:sequence
    [:update {:func #(bt/bb-update % assoc-in (get-path %) initial-state)}]
    [:until-failure (into [:select] states)]]])

(defn end-state
  "Creates an end state. While this node returns a failure, the state machine will succeed."
  ([name]
   [:sequence {:id name} (in-state? name)
    [:failure-leaf]])
  ([name node]
   [:sequence {:id name} (in-state? name)
    node [:failure-leaf]]))

(defn event
  "Creates an event in the hiccup tree. Once the event is triggered, the provided node will be executed."
  [name node]
  [:event name node])

(defn enter-event
  "Creates an enter event in the hiccup tree, triggering as soon as its parent node is transitioned to.
This also trigger when transitioning from the same state, but not when another event of the same state is triggered."
  [node]
  [:enter-event nil node])

(defn state
  "Creates a state, with its associated events"
  ([name & events]
   (let [{:keys [event enter-event]} (group-by first events)
         make-event (fn [[_ event-name node]]
                      [:guard (in-state? name)
                       [:on-event {:event event-name :bind-arg ::event-arg :wait? true :id [:event event-name]}
                        node]])
         events [:until-failure {:id :events-loop}
                 (into [:parallel {:policy :sequence}]
                       (map make-event event))]
         enter-event (if enter-event
                       (if (> (count enter-event) 1)
                         (throw (ex-info "Only one enter-event allowed per state" {}))                         
                         (nth (first enter-event) 2))
                       nil)]
     [:sequence {:id [:state name]}
      (in-state? name)
      (when enter-event
        enter-event)
      (when event
        events)])))

(defn event-arg
  "This context function returns the argument of its closest parent event"
  [ctx]
  (bt/get-var ctx ::event-arg))

(defn transition
  "Transitions to some state, another one or the same."
  [new-state]
  [:sequence
   [:update {:func #(bt/bb-update % assoc-in (get-path %) new-state)}]
   [:failure-leaf]])


