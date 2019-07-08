(ns cark.behavior-tree.register-nodes
  (:require [cark.behavior-tree.node-defs.success-leaf :as success-leaf]
            [cark.behavior-tree.node-defs.failure-leaf :as failure-leaf]
            [cark.behavior-tree.node-defs.inverter :as inverter]
            [cark.behavior-tree.node-defs.sequence :as sequence]
            [cark.behavior-tree.node-defs.select :as select]
            [cark.behavior-tree.node-defs.tick-eater :as tick-eater]
            [cark.behavior-tree.node-defs.update :as update]
            [cark.behavior-tree.node-defs.predicate :as predicate]
            [cark.behavior-tree.node-defs.bind :as bind]
            [cark.behavior-tree.node-defs.parallel :as parallel]
            [cark.behavior-tree.node-defs.send-event :as send-event]
            [cark.behavior-tree.node-defs.consume-event :as consume-event]
            [cark.behavior-tree.node-defs.on-event :as on-event]))

(defn register []
  (success-leaf/register)
  (failure-leaf/register)
  (inverter/register)
  (sequence/register)
  (select/register)
  (tick-eater/register)
  (update/register)
  (predicate/register)
  (bind/register)
  (parallel/register)
  (send-event/register)
  (consume-event/register)
  (on-event/register))
