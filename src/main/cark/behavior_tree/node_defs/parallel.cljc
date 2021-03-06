(ns cark.behavior-tree.node-defs.parallel
  "The :parallel node executes its children in parallel, until it
succeeds or fails according to its policy parameter.

parameters :
- :rerun-children : (default to false) The :success and :failure children are refreshed and rerun while this node is :running
- :policy : (defaults to :sequence) the policy can be :
  - :sequence : the node will succeed when all children succeed, fail when any fails
  - :select : the node will succeed when any child succeeds, fail when all fail
  - a map of the form {:success <value> :failure <value>} : each value can be
     - :every : every node must have the :success or :failure status
     - :some  : some node must have the :success or :failure status
     - an integer : the specified number of children must have this status

Every child node is run before their result is checked against the policy"
  (:require [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.type :as type]
            [cark.behavior-tree.base-nodes :as bn]
            [cark.behavior-tree.hiccup.spec :as hs]
            [clojure.spec.alpha :as s]))

(defn log [value]
  (tap> value)
  value)

(s/def ::policy (s/or :select #(= % :select)
                      :sequence #(= % :sequence)
                      :values ::values))
(s/def ::values (s/keys :req-un [::success ::failure]))
(s/def ::value (s/or :some #(= % :some)
                     :every #(= % :every)
                     :integer (s/and int?
                                     #(> % 0))))
(s/def ::success ::value)
(s/def ::failure ::value)

(s/def ::rerun-children boolean?)

(defn value-policy-func [kw [type value]]
  (let [filter-func #(= % kw)]
    (case type
      :some (fn [children-status]
              (some filter-func children-status))
      :every (fn [children-status]
               (every? filter-func children-status))
      :integer (fn [children-status]
                 (>= (->> children-status
                          (filter filter-func)
                          count)
                     value)))))

(defn values-policy-func [{:keys [success failure] :as policy}]
  (let [success-func (value-policy-func :success success)
        failure-func (value-policy-func :failure failure)
        running-func (fn [children-status]
                       (some #(or (= % :running)
                                  (= % :fresh))
                             children-status))]
    (fn [children-status]
      (cond
        (success-func children-status) :success
        (failure-func children-status) :failure
        (running-func children-status) :running
        :else (throw (ex-info "Undecided parallel result" {:children-status children-status
                                                           :policy policy}))))))

(defn select-policy-func []
  (values-policy-func {:success [:some :some] :failure [:every :every]}))

(defn sequence-policy-func []
  (values-policy-func {:success [:every :every] :failure [:some :some]}))

(defn parsed-policy->func [[type value]]
  (case type
    :select (select-policy-func)
    :sequence (sequence-policy-func)
    :values (values-policy-func value)))

(defn compile-node [tree id tag params children-ids]
  (let [policy-func (if-let [policy (:policy params)]
                      (parsed-policy->func policy)
                      (sequence-policy-func))
        rerun-children (:rerun-children params)]
    [(fn parallel-tick [ctx arg]
       (case (db/get-node-status ctx id)        
         :fresh (recur (db/set-node-status ctx id :running) arg)
         :running (let [ctx (ctx/do-nodes ctx children-ids #(-> (if rerun-children
                                                                  (let [status (db/get-node-status %1 %2)]
                                                                    (if (or (= status :success) (= status :failure))
                                                                      (ctx/set-node-status %1 %2 :fresh)
                                                                      %1))
                                                                  %1)
                                                                (ctx/tick %2)))
                        children-status (mapv #(db/get-node-status ctx %) children-ids)
                        result-status (policy-func children-status)]
                    (case result-status
                      (:success :failure) (-> (db/set-node-status ctx id result-status)
                                              (ctx/reset-nodes children-ids))
                      :running ctx))))
     tree]))

(defn register []
  (type/register
   (bn/branch
    {::type/tag :parallel
     ::type/params-spec (s/? (s/keys :opt-un [::policy ::rerun-children]))
     ::type/children-spec (s/+ ::hs/child)
     ::type/compile-func compile-node})))
