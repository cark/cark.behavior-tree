(ns cark.behavior-tree.state-machine-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.state-machine :as sm]
            [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.hiccup :as h]))

(defn log [value]
  (tap> value)
  value)

(defn is-thrown?
  ([func]
   (is (thrown? #?(:cljs js/Object :clj Exception) (func))))
  ([func text]
   (is (thrown? #?(:cljs js/Object :clj Exception) (func))
       text)))

(defn get-state [ctx]
  (:sm (bt/bb-get ctx)))

(deftest simple-test
  (let [tree (-> [sm/make [:sm] :green
                  (sm/state :green (sm/events (sm/event :advance (sm/transition :yellow))))
                  (sm/state :yellow (sm/events (sm/event :advance (sm/transition :red))))
                  (sm/state :red (sm/events (sm/event :advance (sm/transition :green))))]
                 bt/hiccup->context bt/tick)
        advance (fn [tree] (-> tree (bt/send-event :advance) bt/tick))]
    (is (= :green (-> tree get-state)))
    (is (= :running (-> tree bt/get-status)))
    (is (= :yellow (-> tree advance get-state)))
    (is (= :running (-> tree advance bt/get-status)))
    (is (= :red (-> tree advance advance get-state)))
    (is (= :running (-> tree advance advance bt/get-status)))
    (is (= :green (-> tree advance advance advance get-state)))
    (is (= :running (-> tree advance advance advance bt/get-status)))))

(deftest end-state-test
  (let [tree (-> [sm/make [:sm] :start
                  (sm/state :start (sm/events (sm/event :advance (sm/transition :end))))
                  (sm/end-state :end)]
                 bt/hiccup->context bt/tick)
        advance (fn [tree] (-> tree (bt/send-event :advance) bt/tick))]
    (is (= :start (-> tree get-state)))
    (is (= :running (-> tree bt/get-status)))
    (is (= :end (-> tree advance get-state)))
    (is (= :success (-> tree advance bt/get-status)))))

(deftest with-enter-event+state-test
  (let [tree (-> [:sequence
                  [:update {:func (bt/bb-assocer-in [:val] 0)}]
                  [sm/make [:sm] :start
                   [sm/state :start
                    [sm/events
                     [sm/event :advance (sm/transition :end)]]]
                   [sm/on-enter [:update {:func (bt/bb-updater-in [:val] inc)}]
                    [sm/state :end
                     [sm/events
                      [sm/event :advance [sm/transition :end]]
                      [sm/event :noop [:update {:func identity}]]
                      [sm/event :stop [sm/transition :real-end]]]]]
                   [sm/end-state :real-end]]]
                 bt/hiccup->context bt/tick)
        advance (fn [tree] (-> tree (bt/send-event :advance) bt/tick))
        noop (fn [tree] (-> tree (bt/send-event :noop) bt/tick))
        stop (fn [tree] (-> tree (bt/send-event :stop) bt/tick))]
    (is (= :start (-> tree get-state)))
    (is (= :running (-> tree bt/get-status)))
    (is (= 0 (-> tree bt/bb-get :val)))
    (is (= :end (-> tree advance get-state)))
    (is (= :running (-> tree advance bt/get-status)))
    (is (= :running (-> tree advance advance bt/get-status)))
    (is (= 1 (-> tree advance bt/bb-get :val)))
    (is (= 2 (-> tree advance advance bt/bb-get :val)))
    (is (= :success (-> tree advance advance stop bt/get-status)))
    (is (= :real-end (-> tree advance advance stop get-state)))
    (is (= 1 (-> tree advance noop bt/bb-get :val)))))

