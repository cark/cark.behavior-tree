(ns cark.behavior-tree.state-machine
  (:require [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.hiccup :as hiccup]
            [cark.behavior-tree.event :as event]))

(defn log [value]
  (tap> value)
  value)

(defn- get-path [node & rest]
  (into (bt/get-var node ::path) rest))

(defn- in-state? [name]
  [:predicate {:func #(= (bt/bb-get-in % (get-path %)) name)
               :wait? true}])

(defn make [path initial-state & states]
  [:bind {:let [::path (vec path)]}
   [:sequence
    [:update {:func #(bt/bb-update % assoc-in (get-path %) initial-state)}]
    [:always-success (into [:parallel {:policy :sequence :rerun-children true}] states)]]])

(defn end-state
  ([name]
   [:sequence {:id name} (in-state? name)
    [:failure-leaf]])
  ([name node]
   [:sequence {:id name} (in-state? name)
    node [:failure-leaf]]))

(defn event [name node]
  [:event name node])

(defn enter-event [node]
  [:enter-event nil node])

(defn state
  ([name & events]
   (let [{:keys [event enter-event]} (group-by first events)
         make-event (fn [[_ name node]]
                      [:on-event {:event name :bind-arg ::event-arg :wait? true :id name}
                       node])
         events [:until-failure {:id :events-loop}
                 (into [:parallel {:policy :sequence}]
                       (map make-event event))]
         enter-event (if enter-event
                       (if (> (count enter-event) 1)
                         (throw (ex-info "Only one enter-event per state allowed" {}))                         
                         (nth (first enter-event) 2))
                       nil)]
     [:sequence {:id [:state name]}
      (in-state? name)
      (when enter-event
        enter-event)
      events])))

(defn event-arg [ctx]
  (bt/get-var ctx ::event-arg))

(defn transition [new-state]
  [:sequence
   [:update {:func #(bt/bb-update % assoc-in (get-path %) new-state)}]
   [:failure-leaf]])
