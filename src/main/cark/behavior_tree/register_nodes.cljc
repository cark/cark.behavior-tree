(ns cark.behavior-tree.register-nodes
  (:require [cark.behavior-tree.node-defs.success-leaf :as success-leaf]
            [cark.behavior-tree.node-defs.failure-leaf :as failure-leaf]
            [cark.behavior-tree.node-defs.inverter :as inverter]
            [cark.behavior-tree.node-defs.sequence :as sequence]
            [cark.behavior-tree.node-defs.select :as select]
            [cark.behavior-tree.node-defs.tick-eater :as tick-eater]))

(defn register []
  (success-leaf/register)
  (failure-leaf/register)
  (inverter/register)
  (sequence/register)
  (select/register)
  (tick-eater/register))
