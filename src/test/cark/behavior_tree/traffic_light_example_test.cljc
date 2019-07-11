(ns cark.behavior-tree.traffic-light-example-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.state-machine :as sm]))

;; We define a traffic light changing through all three colors on a fixed schedule

(defn traffic-light-1 []
  (-> [:repeat
       [:sequence
        [:update {:func (bt/bb-setter :green)}]
        [:timer {:timer :traffic-light :duration 50000}]
        [:update {:func (bt/bb-setter :yellow)}]
        [:timer {:timer :traffic-light :duration 10000}]
        [:update {:func (bt/bb-setter :red)}]
        [:timer {:timer :traffic-light :duration 60000}]]]
      bt/hiccup->context (bt/tick 0)))

(defn do-traffic-light-tests [traffic-light]
  (is (= :green (-> traffic-light bt/bb-get)))
  (is (= :green (-> traffic-light (bt/tick+ 49999) bt/bb-get)))
  (is (= :yellow (-> traffic-light (bt/tick+ 50000) bt/bb-get)))
  (is (= :red (-> traffic-light (bt/tick+ 50000) (bt/tick+ 10000) bt/bb-get)))
  (is (= :green (-> traffic-light (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 60000) bt/bb-get)))
  (is (= :yellow (-> traffic-light (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 60000) (bt/tick+ 50000) bt/bb-get)))
  ;;do the same in a single tick (catching up an exceptionally long GC pause)
  (is (= :red (-> traffic-light (bt/tick+ 60000) bt/bb-get)))
  (is (= :green (-> traffic-light (bt/tick+ 120000) bt/bb-get)))
  (is (= :yellow (-> traffic-light (bt/tick+ 170000) bt/bb-get))))

(deftest traffic-light-1-test
  (do-traffic-light-tests (traffic-light-1)))

;; Same traffic lights, using the map node

(defn traffic-light-2 []
  (-> [:repeat
       [:map {:seq [[:green 50000] [:yellow 10000] [:red 60000]] :bind-item :color}
        [:sequence
         [:update {:func #(bt/bb-set % (first (bt/get-var % :color)))}]
         [:timer {:timer :traffic-light :duration #(second (bt/get-var % :color))}]]]]
      bt/hiccup->context (bt/tick 0)))

(deftest traffic-light-2-test
  (do-traffic-light-tests (traffic-light-2)))

;; same thing with a state machine


(defn traffic-light-3 []
  (-> (sm/make [:sm] :green
               (sm/state :green
                         (sm/enter-event [:sequence
                                          [:timer {:timer :traffic-light :duration 50000 :wait? true}]
                                          (sm/transition :yellow)]))
               (sm/state :yellow
                         (sm/enter-event [:sequence
                                          [:timer {:timer :traffic-light :duration 10000 :wait? true}]
                                          (sm/transition :red)]))
               (sm/state :red
                         (sm/enter-event [:sequence
                                          [:timer {:timer :traffic-light :duration 60000 :wait? true}]
                                          (sm/transition :green)])))
      bt/hiccup->context (bt/tick 0)))

(deftest traffic-light-3-test
  (let [get-state (bt/bb-getter-in [:sm])]
    (is (-> (traffic-light-3)))
    (is (= :green (-> (traffic-light-3) get-state)))
    (is (= :yellow (-> (traffic-light-3) (bt/tick+ 50000) get-state)))
    (is (= :running (-> (traffic-light-3) (bt/tick+ 50000) bt/get-status)))
    (is (= :red (-> (traffic-light-3) (bt/tick+ 50000) (bt/tick+ 10000) get-state)))
    (is (= :green (-> (traffic-light-3) (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 60000) get-state)))

    (is (= :red (-> (traffic-light-3) (bt/tick+ 60000) get-state)))
    (is (= :green (-> (traffic-light-3) (bt/tick+ 120000) get-state)))))

;; We define a crossroad with 2 traffic light controllers
;; the :ns north-south controller and the :we west-east controller

(defn make-traffic-light [name time-offset]
  [:sequence
   [:timer {:timer name :duration (- time-offset)}]
   [:repeat
    [:sequence
     [:update {:func (bt/bb-assocer-in [name] :green)}]
     [:timer {:timer name :duration 50000}]
     [:update {:func (bt/bb-assocer-in [name] :yellow)}]
     [:timer {:timer name :duration 10000}]
     [:update {:func (bt/bb-assocer-in [name] :red)}]
     [:timer {:timer name :duration 60000}]]]])

(defn crossroad []
  (-> [:parallel
       (make-traffic-light :we 60000) ;;using the hiccup function call syntax here
       (make-traffic-light :ns 0)]
      bt/hiccup->context (bt/tick 0) (bt/tick+ 60000)))

(deftest crossroad-test
  (let [report (comp (juxt :we :ns) bt/bb-get)]
    (is (= [:green :red] (-> (crossroad) report)))
    (is (= [:yellow :red] (-> (crossroad) (bt/tick+ 50000) report)))
    (is (= [:red :green] (-> (crossroad) (bt/tick+ 50000) (bt/tick+ 10000) report)))
    (is (= [:red :yellow] (-> (crossroad) (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 50000) report)))
    (is (= [:green :red] (-> (crossroad) (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 50000) (bt/tick+ 10000) report)))
    ;; now do it in a single tick
    (is (= [:red :green] (-> (crossroad) (bt/tick+ 60000) report)))
    (is (= [:red :yellow] (-> (crossroad) (bt/tick+ 110000) report)))
    (is (= [:green :red] (-> (crossroad) (bt/tick+ 120000) report)))
    ;; 25-30ms on my computer (jvm), I'll call it good enough
    (let [crossroad (crossroad)]
      (is (= [:green :red] (time (-> crossroad (bt/tick+ (* 1000 60 60 24)) report))))))) 

