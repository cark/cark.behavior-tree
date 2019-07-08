(ns cark.behavior-tree.core
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.hiccup :as h]
            [cark.behavior-tree.register-nodes :as rn]))

(rn/register)

(defn tick [ctx]
  (let [id (tree/get-root-node-id ctx)]
    (-> (ctx/reset-tick-count ctx)
        (ctx/tick id))))

(defn get-status [ctx]
  (db/get-node-status ctx (tree/get-root-node-id ctx)))

(defn hiccup->context
  ([hiccup]
   (hiccup->context ctx/base hiccup))
  ([ctx hiccup]
   (let [parsed (h/parse hiccup)]
     (h/parsed->tree ctx parsed))))
