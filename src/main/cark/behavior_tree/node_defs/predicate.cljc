(ns cark.behavior-tree.node-defs.predicate
  "The :predicate node succeeds or fail depending on its pred parameter.
parameters:
- :func : This context function will extract data from the tree context, before passing it to the :pred function.
- :pred : (defaults to identity) This function will be passed the result of the :func call. The node will succeed if this returns a truthy value, fail otherwise.
- :wait? : The node will stay in the :running state until the predicate is met."
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [clojure.spec.alpha :as s]))

(s/def ::func fn?)
(s/def ::pred ifn?)
(s/def ::wait? (s/or :boolean boolean?
                     :function fn?))

(defn compile-node [tree id tag params _]
  (let [func (:func params)
        [type value] (:wait? params)
        wait? (case type
                :boolean (constantly value)
                :function value
                nil (constantly false))
        pred (or (:pred params) identity)]
    [(fn predicate-tick [ctx arg]
       (if (pred (func ctx))
         (ctx/set-node-status ctx id :success)
         (if (wait? ctx)
           (ctx/set-node-status ctx id :running)
           (ctx/set-node-status ctx id :failure))))
     tree]))

(defn register []
  (type/register
   (bn/leaf
    {::type/tag :predicate
     ::type/params-spec (s/keys :req-un [::func] :opt-un [::wait? ::pred])
     ::type/compile-func compile-node})))
