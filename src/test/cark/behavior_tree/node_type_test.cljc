(ns cark.behavior-tree.node-type-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
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

(deftest tick-eater-cancel-test
  (is (= 1 (let [ctx (-> [:tick-eater {:count 2}]
                         bt/hiccup->context bt/tick)]
             (db/get-node-data ctx (tree/get-root-node-id ctx)))))
  (is (= nil (let [ctx (-> [:tick-eater {:count 2}]
                           bt/hiccup->context bt/tick)
                   id (tree/get-root-node-id ctx)
                   ctx (ctx/set-node-status ctx id :fresh)]
               (db/get-node-data ctx id)))))

(deftest sequence+tick-eater-test
  (is (= :running (-> [:sequence
                       [:success-leaf] [:tick-eater {:count 1}] [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status))))

(deftest update-test
  (is (= :success (-> [:update {:func #(bt/bb-set % 1)}] bt/hiccup->context bt/tick bt/get-status)))
  (is (= 1 (-> [:update {:func #(bt/bb-set % 1)}] bt/hiccup->context bt/tick bt/bb-get)))
  (is (= 2 (-> [:sequence
                [:update {:func (bt/bb-setter 1)}]
                [:update {:func (bt/bb-updater inc)}]]
               bt/hiccup->context bt/tick bt/bb-get)))
  (is (= 2 (-> [:sequence
                [:update {:func (bt/bb-updater-in [:a :b] (constantly 1))}] 
                [:update {:func (bt/bb-updater-in [:a :b] inc)}]]
               bt/hiccup->context bt/tick (bt/bb-get-in [:a :b])))))

(deftest predicate-no-wait-test
  (is (= :failure (-> [:predicate {:func (bt/bb-getter-in [:a])}] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:sequence
                       [:update {:func (bt/bb-updater-in [:a] (constantly true))}]
                       [:predicate {:func (bt/bb-getter-in [:a])}]] bt/hiccup->context bt/tick bt/get-status))))

(deftest predicate-wait-test
  (is (= :running (-> [:predicate {:func (bt/bb-getter-in [:a]) :wait? true}] bt/hiccup->context
                      bt/tick bt/get-status)))
  (is (= :success (-> [:predicate {:func (bt/bb-getter-in [:a]) :wait? true}] bt/hiccup->context
                      bt/tick (bt/bb-update-in [:a] (constantly true)) bt/tick bt/get-status))))

(deftest bind-test
  (is (= :success (-> [:bind {:let [:a 1]}
                       [:predicate {:func #(= 1 (bt/get-var % :a))}]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:bind {:let [:a 2]}
                       [:predicate {:func #(= 1 (bt/get-var % :a))}]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:bind {:let [:a (constantly 1)]}
                       [:predicate {:func #(= 1 (bt/get-var % :a))}]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:bind {:let [:a (constantly 2)]}
                       [:predicate {:func #(= 1 (bt/get-var % :a))}]]
                      bt/hiccup->context bt/tick bt/get-status))))

(deftest bind-shadowing-test
  (is (= :success (-> [:bind {:let [:a 1 :b 2]}
                       [:bind {:let [:b 3]}
                        [:predicate {:func #(= (bt/get-var % :b) 3)}]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:bind {:let [:a 1 :b 2]}
                       [:bind {:let [:b 3]}
                        [:predicate {:func #(= (bt/get-var % :a) 1)}]]]
                      bt/hiccup->context bt/tick bt/get-status))))

(deftest parallel-test
  (is-thrown? #(-> [:parallel] bt/hiccup->context)) 
  (is (= :success (-> [:parallel {:policy :select}
                       [:success-leaf] [:failure-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :select}
                       [:failure-leaf] [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:parallel [:success-leaf] [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:parallel [:failure-leaf] [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:parallel [:success-leaf] [:failure-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :sequence} [:success-leaf] [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:parallel {:policy :sequence} [:failure-leaf] [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:parallel {:policy :sequence} [:success-leaf] [:failure-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= 2 (-> [:sequence
                [:update {:func #(bt/bb-set % 0)}]
                [:parallel {:policy :select}
                 [:success-leaf]
                 [:update {:func #(bt/bb-update % inc)}]
                 [:update {:func #(bt/bb-update % inc)}]]]
               bt/hiccup->context bt/tick bt/bb-get))))

(deftest send-event-test
  (is (= :success (-> [:send-event {:event :bar}] bt/hiccup->context bt/tick bt/get-status)))
  (is (= [[:bar nil]] (-> [:send-event {:event :bar}] bt/hiccup->context bt/tick bt/get-events)))
  (is (= [[:bar :baz]] (-> [:send-event {:event :bar
                                         :arg (constantly :baz)}]
                           bt/hiccup->context bt/tick bt/get-events)))
  (is (= [[:bar nil] [:baz nil]] (-> [:sequence
                                      [:send-event {:event :bar}]
                                      [:send-event {:event :baz}]]
                                     bt/hiccup->context bt/tick bt/get-events)))
  (is (= [[:bar nil] [:baz nil]] (-> [:sequence
                                      [:send-event {:event :bar}]
                                      [:send-event {:event :baz}]]
                                     bt/hiccup->context bt/tick bt/get-events)))
  (is (= [[:bar nil]] (-> [:sequence
                           [:send-event {:event (constantly :bar)}]
                           [:tick-eater {:count 1}]
                           [:send-event {:event (constantly :baz)}]]
                          bt/hiccup->context bt/tick bt/get-events)))
  (is (= [[:baz nil]] (-> [:sequence
                           [:send-event {:event (constantly :bar)}]
                           [:tick-eater {:count (constantly 1)}]
                           [:send-event {:event (constantly :baz)}]]
                          bt/hiccup->context bt/tick bt/tick bt/get-events))))


(deftest consume-event-test
  (-> [:consume-event {:event (constantly :bar)}] bt/hiccup->context bt/tick)
  (is (= :failure (-> [:consume-event {:event (constantly :bar)}] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:consume-event {:event (constantly :bar)}] bt/hiccup->context
                      (bt/send-event :bar) bt/tick bt/get-status)))
  (is-thrown? #(-> [:consume-event {:event (constantly :bar)}] bt/hiccup->context
                   (bt/send-event :foo) bt/tick bt/get-status))
  (is-thrown? #(-> [:consume-event {:event (constantly :bar)
                                    :pick? (fn [ctx arg] (= arg 1))}] bt/hiccup->context
                   (bt/send-event :bar) bt/tick bt/get-status))
  (is (= :success (-> [:consume-event {:event (constantly :bar)
                                       :pick? (fn [ctx arg] (= arg 1))}] bt/hiccup->context
                      (bt/send-event :bar 1) bt/tick bt/get-status)))
  (is (= 1 (-> [:consume-event {:event (constantly :bar)
                                :with-arg (fn [ctx arg] (bt/bb-set ctx arg))}] bt/hiccup->context
               (bt/send-event :bar 1) bt/tick bt/bb-get)))
  (is (= :running (-> [:consume-event {:event (constantly :foo)
                                       :wait? (constantly true)}]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:consume-event {:event :foo
                                       :wait? true}]
                      bt/hiccup->context bt/tick (bt/send-event :foo) bt/tick bt/get-status))))

(deftest on-event-no-wait-test
  (is (= :failure (-> [:on-event {:event :foo :bind-arg :a}
                       [:success-leaf]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:on-event {:event :foo :bind-arg :a}
                       [:success-leaf]] bt/hiccup->context
                      (bt/send-event :foo) bt/tick bt/get-status)))
  (is (= 1 (-> [:on-event {:event :foo :bind-arg :a}
                [:update {:func (bt/bb-setter (bt/var-getter :a))}]] bt/hiccup->context
               (bt/send-event :foo 1) bt/tick bt/bb-get))))

(deftest on-event-wait-test
  (is (= :running (-> [:on-event {:event :foo :bind-arg :a :wait? true}
                       [:success-leaf]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:on-event {:event :foo :bind-arg :a :wait? true}
                       [:success-leaf]] bt/hiccup->context
                      bt/tick (bt/send-event :foo) bt/tick bt/get-status)))
  (is (= 1 (-> [:on-event {:event :foo :bind-arg :a :wait? true}
                [:update {:func #(bt/bb-set % (bt/get-var % :a))}]] bt/hiccup->context
               bt/tick (bt/send-event :foo 1) bt/tick bt/bb-get))))