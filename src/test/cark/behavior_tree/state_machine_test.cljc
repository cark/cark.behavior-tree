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
  (:state (:sm (bt/bb-get ctx))))

(deftest simple-test
  (let [tree (-> (sm/make [:sm] :green
                   (sm/state :green
                     (sm/event :advance
                       (sm/transition :yellow)))
                   (sm/state :yellow
                     (sm/event :advance (sm/transition :red)))
                   (sm/state :red
                     (sm/event :advance (sm/transition :green))))
                 bt/hiccup->context bt/tick)
        advance (fn [tree] (-> tree (bt/send-event :advance)))]
    (is (= :green (-> tree get-state)))
    (is (= :running (-> tree bt/get-status)))
    (is (= :yellow (-> tree advance get-state)))
    (is (= :running (-> tree advance bt/get-status)))
    (is (= :red (-> tree advance advance get-state)))
    (is (= :running (-> tree advance advance bt/get-status)))
    (is (= :green (-> tree advance advance advance get-state)))
    (is (= :running (-> tree advance advance advance bt/get-status)))))

(deftest end-state-test
  (let [tree (-> (sm/make [:sm] :start
                   (sm/state :start (sm/event :advance (sm/transition :end)))
                   (sm/end-state :end))
                 bt/hiccup->context bt/tick)
        advance (fn [tree] (-> tree (bt/send-event :advance)))]
    (is (= :start (-> tree get-state)))
    (is (= :running (-> tree bt/get-status)))
    (is (= :end (-> tree advance get-state)))
    (is (= :success (-> tree advance bt/get-status)))))

(deftest with-enter-event+state-test
  (let [tree (-> [:sequence
                  [:update {:func (bt/bb-assocer-in [:val] 0)}]
                  (sm/make [:sm] :start
                    (sm/state :start
                      (sm/event :advance (sm/transition :end)))
                    (sm/state :end
                      (sm/enter-event [:update {:func (bt/bb-updater-in [:val] inc)
                                                :id :increment-here!}])
                      (sm/event :advance (sm/transition :end))
                      (sm/event :noop [:update {:func identity}])
                      (sm/event :stop (sm/transition :real-end)))
                    (sm/end-state :real-end))]
                 bt/hiccup->context bt/tick)
        advance (fn [tree] (-> tree (bt/send-event :advance)))
        noop (fn [tree] (-> tree (bt/send-event :noop)))
        stop (fn [tree] (-> tree (bt/send-event :stop)))]
    (is (= :start (-> tree get-state)))
    (is (= :running (-> tree bt/get-status)))
    (is (= 0 (-> tree bt/bb-get :val)))
    (is (= :end (-> tree advance get-state)))
    (is (= :running (-> tree advance bt/get-status)))
    (is (= :running (-> tree advance advance bt/get-status)))
    (is (= 1 (-> tree advance bt/bb-get :val)))
    (is (= 2 (-> tree advance advance bt/bb-get :val)))
    (is (= :real-end (-> tree advance advance stop get-state)))
    (is (= :success (-> tree advance advance stop bt/get-status))) 
    (is (= 1 (-> tree advance noop bt/bb-get :val)))))

(deftest change-state-then-event-also-in-old-state-test
  (let [ctx (-> (sm/make [:sm] :start
                  (sm/state :start
                    (sm/event :foo (sm/transition :bar))
                    (sm/event :baz [:send-event {:event :start-baz}]))
                  (sm/state :bar
                    (sm/event :baz [:send-event {:event :bar-baz}])))
                bt/hiccup->context bt/tick)]
    (is (= :running (bt/get-status ctx)))
    (is (= :start (get-state ctx)))
    (is (= :bar (-> ctx (bt/send-event :foo) get-state)))
    (is (= [[:start-baz nil]] (-> (bt/send-event ctx :baz) bt/get-events)))
    (is (= [[:bar-baz nil]] (-> ctx (bt/send-event :foo) (bt/send-event :baz) bt/get-events)))))

(deftest parallel-test
  (let [ctx (-> [:parallel {:policy :select}
                 (sm/make [:aba] :a
                   (sm/state :a (sm/event :a (sm/transition :ab)))
                   (sm/state :ab (sm/event :b (sm/transition :aba)))
                   (sm/state :aba (sm/event :a (sm/transition :end)))
                   (sm/end-state :end))
                 (sm/make [:abb] :a
                   (sm/state :a (sm/event :a (sm/transition :ab)))
                   (sm/state :ab (sm/event :b (sm/transition :abb)))
                   (sm/state :abb (sm/event :b (sm/transition :end)))
                   (sm/end-state :end))]
                bt/hiccup->context bt/tick)]
    (is (= :running (-> ctx bt/get-status)))
    (is (= :success (-> ctx
                        (bt/send-event :a)
                        (bt/send-event :b)
                        (bt/send-event :a)
                        bt/get-status)))
    (is (= :success (-> ctx
                        (bt/send-event :a)
                        (bt/send-event :b)
                        (bt/send-event :b)
                        bt/get-status)))
    (is (= :running (-> ctx
                        (bt/send-event :a)
                        (bt/send-event :b)
                        (bt/send-event :c)
                        bt/get-status)))))

(deftest hierachy-test
  (let [b-machine (sm/make [:b] :b
                    (sm/state :b
                      (sm/enter-event [:send-event {:event :entered-b-b}])
                      (sm/event :c (sm/transition :c)))
                    (sm/state :c
                      (sm/enter-event [:send-event {:event :entered-b-c}])
                      (sm/event :d (sm/transition :d)))
                    (sm/end-state :d
                      [:send-event {:event :entered-b-d}]))
        ctx (-> (sm/make [:a] :a
                  (sm/state :a
                    (sm/enter-event
                     [:sequence
                      [:send-event {:event :entered-a-a}]
                      b-machine
                      [:send-event {:event :entered-a-a-after}]])
                    (sm/event :e
                      (sm/transition :e)))
                  (sm/state :e
                    (sm/enter-event [:send-event {:event :entered-a-e}])))
                bt/hiccup->context bt/tick)]
    (is (= :running (-> ctx bt/get-status)))
    (is (= [[:entered-a-a nil]
            [:entered-b-b nil]]
           (-> ctx bt/get-events)))
    (is (= [[:entered-b-c nil]]
           (-> ctx (bt/send-event :c) bt/get-events)))
    (is (= [[:entered-b-d nil]
            [:entered-a-a-after nil]]
           (-> ctx (bt/send-event :c) (bt/send-event :d) bt/get-events)))
    (is (= [[:entered-a-e nil]]
           (-> ctx (bt/send-event :e) bt/get-events)))))
