(ns cark.behavior-tree.traffic-light-example-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]))

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

(deftest traffic-light-1-test
  (is (= :green (-> (traffic-light-1) bt/bb-get)))
  (is (= :green (-> (traffic-light-1) (bt/tick+ 49999) bt/bb-get)))
  (is (= :yellow (-> (traffic-light-1) (bt/tick+ 50000) bt/bb-get)))
  (is (= :red (-> (traffic-light-1) (bt/tick+ 50000) (bt/tick+ 10000) bt/bb-get)))
  (is (= :green (-> (traffic-light-1) (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 60000) bt/bb-get)))
  (is (= :yellow (-> (traffic-light-1) (bt/tick+ 50000) (bt/tick+ 10000) (bt/tick+ 60000) (bt/tick+ 50000) bt/bb-get)))
  ;;do the same in a single tick (catching up an exceptionally long GC pause)
  (is (= :red (-> (traffic-light-1) (bt/tick+ 60000) bt/bb-get)))
  (is (= :green (-> (traffic-light-1) (bt/tick+ 120000) bt/bb-get)))
  (is (= :yellow (-> (traffic-light-1) (bt/tick+ 170000) bt/bb-get))))


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
       (make-traffic-light :we 60000)
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
    ;; 30ms on my computer (jvm), I'll call it good enough
    (is (= [:green :red] (time (-> (crossroad) (bt/tick+ (* 1000 60 60 24)) report)))))) 

