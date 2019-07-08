(ns cark.behavior-tree.node-type-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.tree :as tree]))

(defn log [value]
  (tap> value)
  value)

(defn is-thrown?
  ([func]
   (is (thrown? #?(:cljs js/Object :clj Exception) (func))))
  ([func text]
   (is (thrown? #?(:cljs js/Object :clj Exception) (func))
       text)))

(deftest success-leaf-test
  (let [ctx (-> [:success-leaf] bt/hiccup->context)]
    (is ctx)
    (is (= :success (-> ctx bt/tick bt/get-status)))
    (is (= 1 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count))))) 

(deftest failure-leaf-test
  (let [ctx (-> [:failure-leaf] bt/hiccup->context)]
    (is ctx)
    (is (= :failure (-> ctx bt/tick bt/get-status)))
    (is (= 1 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count))))) 

(deftest inverter-success-test
  (let [ctx (-> [:inverter [:success-leaf]] bt/hiccup->context)]
    (is (= :success (-> ctx bt/tick bt/get-status)))
    (is (= 2 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= :success (-> ctx bt/tick bt/tick bt/get-status)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count)))))

(deftest inverter-failure-test
  (let [ctx (-> [:inverter [:failure-leaf]] bt/hiccup->context)]
    (is (= :failure (-> ctx bt/tick bt/get-status)))
    (is (= 2 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= :failure (-> ctx bt/tick bt/tick bt/get-status)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count)))))

(deftest sequence-test
  (let [ctx (-> [:sequence [:success-leaf] [:success-leaf]] bt/hiccup->context)]
    (is (= :success (-> ctx bt/tick bt/get-status)))
    (is (= 3 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count))))
  (let [ctx (-> [:sequence [:success-leaf] [:failure-leaf]] bt/hiccup->context)]
    (is (= :failure (-> ctx bt/tick bt/get-status)))
    (is (= 3 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count))))
  (let [ctx (-> [:sequence [:failure-leaf] [:success-leaf]] bt/hiccup->context)]
    (is (= :failure (-> ctx bt/tick bt/get-status)))
    (is (= 2 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count)))))

(deftest select-test
  (let [ctx (-> [:select [:failure-leaf] [:failure-leaf]] bt/hiccup->context)]
    (is (= :failure (-> ctx bt/tick bt/get-status)))
    (is (= 3 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count))))
  (let [ctx (-> [:select [:success-leaf] [:failure-leaf]] bt/hiccup->context)]
    (is (= :success (-> ctx bt/tick bt/get-status)))
    (is (= 2 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count))))
  (let [ctx (-> [:select [:failure-leaf] [:success-leaf]] bt/hiccup->context)]
    (is (= :success (-> ctx bt/tick bt/get-status)))
    (is (= 3 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count)))))

(deftest tick-eater-literal-test
  (let [ctx (-> [:tick-eater {:count 2}] bt/hiccup->context)]
    (is (= :running (-> ctx bt/tick bt/get-status)))
    (is (= :running (-> ctx bt/tick bt/tick bt/get-status)))
    (is (= :success (-> ctx bt/tick bt/tick bt/tick bt/get-status))))
  (is (= :success (-> [:tick-eater {:count 0}] bt/hiccup->context bt/tick bt/get-status))))

(deftest tick-eater-function-test
  (let [ctx (-> [:tick-eater {:count (constantly 2)}] bt/hiccup->context)]
    (is (= :running (-> ctx bt/tick bt/get-status))) 
    (is (= :running (-> ctx bt/tick bt/tick bt/get-status)))
    (is (= :success (-> ctx bt/tick bt/tick bt/tick bt/get-status))))
  (is (= :success (-> [:tick-eater {:count (constantly 0)}] bt/hiccup->context bt/tick bt/get-status))))
