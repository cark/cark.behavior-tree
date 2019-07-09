(ns cark.behavior-tree.state-machine
  (:require [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.hiccup :as hiccup]))

(defn log [value]
  (tap> value)
  value)

(defn- get-path [node & rest]
  (into (bt/get-var node ::path) rest))

(defn- in-state? [name]
  [:predicate {:func #(= (bt/bb-get-in % (get-path %)) name)}])

(defn state-machine [path initial-state & states]
  [:bind {:let [::path (vec path)]}
   [:sequence
    [:update {:func #(bt/bb-update % assoc-in (get-path %) initial-state)}]
    [:until-failure
     (into [:parallel {:policy :select}] states)]]])

(defn end-state
  ([name]
   [:guard (in-state? name)
    [:failure-leaf]])
  ([name node]
   [:guard (in-state? name)
    [:sequence node [:failure-leaf]]]))

(defn state
  ([name node-or-events]
   [:guard (in-state? name) [:always-success node-or-events]])
  ([name node events]
   [:guard (in-state? name) [:sequence [:always-success node] events]]))

(defn events [& events]
  [:until-failure
   (into [:parallel {:policy {:success 1 :failure 1}}] events)])

(defn on-enter [node state]
  (let [[_ pred pl] (hiccup/prepare state)]
    [:guard pred [:sequence [:always-success node] pl]]))

(defn event-arg [ctx]
  (bt/get-var ctx ::event-arg))

(defn event [name node]
  [:on-event {:event name :bind-arg ::event-arg :wait? true}
   node])

(defn transition [new-state]
  [:sequence
   [:update {:func #(bt/bb-update % assoc-in (get-path %) new-state)}]
   [:failure-leaf]])


