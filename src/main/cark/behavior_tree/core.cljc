(ns cark.behavior-tree.core
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.hiccup :as h]
            [cark.behavior-tree.dynamic-extent :as de]
            [cark.behavior-tree.register-nodes :as rn]
            [cark.behavior-tree.event :as event]))

(defn- log [value]
  (tap> value)
  value)

(rn/register)

(defn- check-no-in-events-left [ctx]
  (if (-> ctx event/get-events-in seq)
    (throw (ex-info "Some events were not consumed." {:events (-> ctx event/get-events-in)}))
    ctx))

(defn tick
  "Ticks the tree, going through all running and fresh nodes in a depth first manner. The optional 
time parameter specifies at which time (in milliseconds) the tick is supposed to be occuring. This time parameter is usefull for testing."
  ([ctx]
   (tick ctx #?(:cljs (js/Date.now)
                :clj (System/currentTimeMillis))))
  ([ctx time]
   (let [id (tree/get-root-node-id ctx)]
     (-> (ctx/reset-tick-count ctx)
         event/clear-events-out
         (db/set-time time)
         (ctx/tick id)
         check-no-in-events-left))))

(defn tick+
  "This function is for testing purpose. It ticks the tree 'duration' milliseconds after it was last ticked."
  [ctx duration]
  (tick ctx (+ (db/get-time ctx) duration)))

(defn get-status
  "Returns the status of the root node of the tree"
  [ctx]
  (db/get-node-status ctx (tree/get-root-node-id ctx)))

(defn hiccup->context
  "Takes an hissup behavior tree definition and returns a behavior tree context, ready to be ticked."
  ([hiccup]
   (hiccup->context ctx/base hiccup))
  ([ctx hiccup]
   (let [parsed (h/parse hiccup)]
     (h/parsed->tree ctx parsed))))

(defn bb-get
  "Returns the blackboard of this tree"
  [ctx]
  (db/get-blackboard ctx))

(defn bb-get-in
  "Returns a value from the blackboard of this tree. The path parameter is like the one of get-in."
  [ctx path]
  (get-in (db/get-blackboard ctx) path))

(defn bb-update
  "Update the blackboard of this tree, applying the provided func with the blackboard as its first parameter
and the params as the rest."
  [ctx func & params]
  (db/update-blackboard ctx #(apply func % params )))

(defn bb-update-in
  "Updates the blackboard of this tree, with the func and path specified."
  [ctx path func & params]
  (if (seq path)
    (db/update-blackboard ctx #(apply update-in % path func params))
    (db/update-blackboard ctx #(apply func % params))))

(defn bb-set
  "Sets the whole blackboard of this tree in one go."
  [ctx value]
  (db/set-blackboard ctx value))

(defn bb-updater-in
  "Returns a function that can be applied to a tree. It will update its blackboard at the provided path, with the provided funtion
 and arguments"
  [path func & params]
  (if (seq path)
    (fn [ctx]
      (db/update-blackboard ctx #(apply update-in % path func params)))
    (fn [ctx]
      (db/update-blackboard ctx #(apply func % params)))))

(defn bb-updater
  "Returns a function that can be applied to a tree. It will update its blackboard with the provided function and arguments"
  [func & params]
  (fn [ctx]
    (db/update-blackboard ctx #(apply func % params))))

(defn bb-getter
  "Returns a function that can be applied to a tree, returning its blackboard."
  []
  (fn [ctx]
    (db/get-blackboard ctx)))

(defn bb-getter-in
  "Returns a function that can be applied to a tree, returning the value stored at the specified path in its blackboard"
  [path]
  (fn [ctx]
    (get-in (db/get-blackboard ctx) path)))

(defn bb-setter
  "Returns a function that can be applied to a tree, setting the provided value as its new blackboard."
  [value]
  (if (fn? value)
    (fn [ctx]
      (db/set-blackboard ctx (value ctx)))
    (fn [ctx]
      (db/set-blackboard ctx value))))

(defn bb-assocer-in
  "Returns a function that can be applied to a tree, associating the provided value to the specified path in its blackboard."
  [path value]
  (if (fn? value)
    (fn [ctx]
      (db/update-blackboard ctx #(assoc-in % path (value ctx))))
    (fn [ctx]
      (db/update-blackboard ctx #(assoc-in % path value)))))

(defn get-var
  "Returns the value a variable previously bound, for instance by the :bind node"
  [ctx name]
  (de/get-var ctx name))

(defn var-getter
  "Returns a function that can be applied to a tree, returning value of the variable with the provided name"
  [name]
  (fn [ctx]
    (de/get-var ctx name)))

(defn get-events
  "Returns a vector of all the events that were raised during the last tick of the tree. Each event is a vector pair
with the event name as its first element, and the event argument as its second element, or nil if there is none."
  [ctx]
  (event/get-events-out ctx))

(defn send-event
  "Sends an event to the tree, that can later be consumed by a :consume-event or :on-event node. An optional
arg parameter can be sent along with it. "
  ([ctx name]
   (send-event ctx name nil))
  ([ctx name arg]
   (event/add-event-in ctx name arg)))

(defn extract-db
  "Extracts the database of this tree. It contains all the book-keeping transient data for the tree, as well as it blackboard.
This is a O(1) operation."
  [ctx]
  (db/extract ctx))

(defn merge-db
  "Merge the provided database with the tree. This is a O(1) operation."
  [ctx db]
  (merge ctx db))
