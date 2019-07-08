(ns cark.behavior-tree.core
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.hiccup :as h]
            [cark.behavior-tree.lexical-context :as lc]
            [cark.behavior-tree.register-nodes :as rn]
            [cark.behavior-tree.event :as event]))

(defn log [value]
  (tap> value)
  value)

(rn/register)

(defn- check-no-in-events-left [ctx]
  (if (-> ctx event/get-events-in seq)
    (throw (ex-info "Some events were not consumed." {:events (-> ctx event/get-events-in)}))
    ctx))

(defn tick [ctx]
  (let [id (tree/get-root-node-id ctx)]
    (-> (ctx/reset-tick-count ctx)
        event/clear-events-out
        (ctx/tick id)
        check-no-in-events-left)))

(defn get-status [ctx]
  (db/get-node-status ctx (tree/get-root-node-id ctx)))

(defn hiccup->context
  ([hiccup]
   (hiccup->context ctx/base hiccup))
  ([ctx hiccup]
   (let [parsed (h/parse hiccup)]
     (h/parsed->tree ctx parsed))))

(defn bb-get [ctx]
  (db/get-blackboard ctx))

(defn bb-get-in [ctx path]
  (get-in (db/get-blackboard ctx) path))

(defn bb-update [ctx func & params]
  (db/update-blackboard ctx #(apply func % params )))

(defn bb-update-in [ctx path func & params]
  (if (seq path)
    (db/update-blackboard ctx #(apply update-in % path func params))
    (db/update-blackboard ctx #(apply func % params))))

(defn bb-set [ctx value]
  (db/set-blackboard ctx value))

(defn bb-updater-in [path func & params]
  (if (seq path)
    (fn [ctx]
      (db/update-blackboard ctx #(apply update-in % path func params)))
    (fn [ctx]
      (db/update-blackboard ctx #(apply func % params)))))

(defn bb-updater [func & params]
  (fn [ctx]
    (db/update-blackboard ctx #(apply func % params))))

(defn bb-getter []
  (fn [ctx]
    (db/get-blackboard ctx)))

(defn bb-getter-in [path]
  (fn [ctx]
    (get-in (db/get-blackboard ctx) path)))

(defn bb-setter [value]
  (if (fn? value)
    (fn [ctx]
      (db/set-blackboard ctx (value ctx)))
    (fn [ctx]
      (db/set-blackboard ctx value))))

(defn bb-assocer-in [path value]
  (if (fn? value)
    (fn [ctx]
      (db/update-blackboard ctx #(assoc-in % path (value ctx))))
    (fn [ctx]
      (db/update-blackboard ctx #(assoc-in % path value)))))

(defn get-var [ctx name]
  (lc/get-var ctx name))

(defn var-getter [name]
  (fn [ctx]
    (lc/get-var ctx name)))

(defn get-events [ctx]
  (event/get-events-out ctx))

(defn send-event
  ([ctx name]
   (send-event ctx name nil))
  ([ctx name arg]
   (event/add-event-in ctx name arg)))
