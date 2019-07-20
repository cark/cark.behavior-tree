(ns ^:no-doc cark.behavior-tree.register-nodes
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
            #_[cark.behavior-tree.node-defs.consume-event :as consume-event]
            [cark.behavior-tree.node-defs.on-event :as on-event]
            [cark.behavior-tree.node-defs.guard :as guard]
            [cark.behavior-tree.node-defs.guard-selector :as guard-selector]
            [cark.behavior-tree.node-defs.repeat :as repeat]
            [cark.behavior-tree.node-defs.until-success :as until-success]
            [cark.behavior-tree.node-defs.until-failure :as until-failure]
            [cark.behavior-tree.node-defs.timer :as timer]
            [cark.behavior-tree.node-defs.always-success :as always-success]
            [cark.behavior-tree.node-defs.always-failure :as always-failure]
            [cark.behavior-tree.node-defs.map :as map]
            [cark.behavior-tree.node-defs.on-cancel :as on-cancel]
            [cark.behavior-tree.node-defs.trace :as trace]
            [cark.behavior-tree.node-defs.timer-init :as timer-init]))

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
  ;;(consume-event/register)
  (on-event/register)
  (guard/register)
  (guard-selector/register)
  (repeat/register)
  (until-success/register)
  (until-failure/register)
  (timer/register)
  (always-success/register)
  (always-failure/register)
  (map/register)
  (on-cancel/register)
  (trace/register)
  (timer-init/register)) 
