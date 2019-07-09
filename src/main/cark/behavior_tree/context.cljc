(ns cark.behavior-tree.context
  (:refer-clojure :exclude [keys])
  (:require [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
            [cark.behavior-tree.lexical-context :as lc]
            [clojure.set :as set]))

(def default-max-tick-count 100000)

(def specific-keys
  #{::max-tick-count ::tick-count ::time})

(def keys
  (set/union specific-keys db/keys tree/keys lc/keys))

(defn make [db tree]
  (merge {::max-tick-count default-max-tick-count
          ::tick-count 0
          ::time 0}
         db tree (lc/make)))

(def base (make (db/make) (tree/make)))

(defn get-max-tick-count [context]
  (::max-tick-count context))

(defn set-max-tick-count [context value]
  (assoc context ::max-tick-count value))

(defn reset-tick-count [context]
  (assoc context ::tick-count 0))

(defn get-tick-count [context]
  (::tick-count context))

(defn inc-tick-count [context]
  (if (< (get-tick-count context) (get-max-tick-count context))
    (update context ::tick-count inc)
    (throw (ex-info "Max tick count reached" {:tick-count (get-tick-count context)}))))

(defn get-time
  "Returns the time at which this context is executing, as system milliseconds"
  [ctx]
  (::time ctx))

(defn set-time
  "Sets the context's execution time, as system milliseconds"
  [ctx ms]
  (assoc ctx ::time ms))

(defn tick [ctx node-id]
  (case (db/get-node-status ctx node-id)
    (:fresh :running) ((tree/get-node ctx node-id) (inc-tick-count ctx) nil)
    (:success :failure) ctx))

(declare cancel)

(defn set-node-status [ctx node-id new-status]
  (let [old-status (db/get-node-status ctx node-id)]
    (-> (if (and (= :fresh new-status)
                 (= :running old-status))
          (cancel ctx node-id)
          ctx)
        (db/set-node-status node-id new-status))))

(defn do-nodes [ctx node-ids func]
  (reduce (fn [ctx node-id]
            (func ctx node-id))
          ctx node-ids))

(defn reset-nodes [ctx node-ids]
  (do-nodes ctx node-ids #(set-node-status %1 %2 :fresh)))

(defn get-node-children-ids [ctx node-id]
  (:children-ids (tree/get-node-meta ctx node-id)))

(defn cancel [ctx node-id]
  ;; TODO: tick cancel node ...cancelling should remove node data as well ..not that's a bug !
  #_(reset-nodes ctx (call-node ctx node-id :get-children-ids nil))
  (-> (reset-nodes ctx (get-node-children-ids ctx node-id))
      (db/set-node-data node-id nil)))


