(ns cark.behavior-tree.node-type-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.context :as ctx]
            [cark.behavior-tree.db :as db]
            [cark.behavior-tree.tree :as tree]
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
    (is (= :failure (-> ctx bt/tick bt/get-status)))
    (is (= 2 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= :failure (-> ctx bt/tick bt/tick bt/get-status)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count)))))

(deftest inverter-failure-test
  (let [ctx (-> [:inverter [:failure-leaf]] bt/hiccup->context)]
    (is (= :success (-> ctx bt/tick bt/get-status)))
    (is (= 2 (-> ctx bt/tick ctx/get-tick-count)))
    (is (= :success (-> ctx bt/tick bt/tick bt/get-status)))
    (is (= 0 (-> ctx bt/tick bt/tick ctx/get-tick-count)))))

(deftest inverter+tick-eater-test
  (is (= :running (-> [:inverter [:tick-eater {:count 1}]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:inverter [:tick-eater {:count 1}]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status))))

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
             (db/get-node-data ctx (tree/get-root-node-id ctx))))))

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
  (is (= :success (-> [:predicate {:func (bt/bb-getter-in [:a]) :pred complement}] bt/hiccup->context bt/tick bt/get-status)))
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

(deftest parallel-rerun-children-test
  (is (= :running (-> [:parallel {:policy :sequence}
                       [:update {:func (bt/bb-updater inc)}]
                       [:predicate {:func #(> (bt/bb-get %) 1) :wait? true}]]
                      bt/hiccup->context (bt/bb-set 0) bt/tick bt/get-status)))
  (is (= :running (-> [:parallel {:policy :sequence}
                       [:update {:func (bt/bb-updater inc)}]
                       [:predicate {:func #(> (bt/bb-get %) 1) :wait? true}]]
                      bt/hiccup->context (bt/bb-set 0) bt/tick bt/tick bt/get-status)))
  (is (= :running (-> [:parallel {:policy :sequence :rerun-children true}
                       [:update {:func (bt/bb-updater inc)}]
                       [:predicate {:func #(> (bt/bb-get %) 1) :wait? true}]]
                      bt/hiccup->context (bt/bb-set 0) bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :sequence :rerun-children true}
                       [:update {:func (bt/bb-updater inc)}]
                       [:predicate {:func #(> (bt/bb-get %) 1) :wait? true}]]
                      bt/hiccup->context (bt/bb-set 0) bt/tick bt/tick bt/get-status))))

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

(deftest guard-test
  (is (= :failure (-> [:guard [:failure-leaf] [:tick-eater {:count 5}]] bt/hiccup->context
                      bt/tick bt/get-status)))
  (is (= :running (-> [:guard [:success-leaf] [:tick-eater {:count 1}]] bt/hiccup->context
                      bt/tick bt/get-status)))
  (is (= :success (-> [:guard [:success-leaf] [:tick-eater {:count 1}]] bt/hiccup->context
                      bt/tick bt/tick bt/get-status)))
  (is (= :success (-> [:guard [:predicate {:func (bt/bb-getter)}]
                       [:tick-eater {:count (constantly 1)}]] bt/hiccup->context
                      bt/tick bt/tick bt/get-status)))
  (let [ctx (-> [:guard [:predicate {:func (bt/bb-getter)}]
                 [:tick-eater {:count 2}]] bt/hiccup->context
                bt/tick (bt/bb-set nil) bt/tick)]
    (is (= :failure (-> ctx bt/get-status)))
    (is (= :fresh (db/get-node-status ctx (get (ctx/get-node-children-ids ctx (tree/get-root-node-id ctx)) 1)))))
  (is-thrown? #(-> [:guard] bt/hiccup->context))
  (is-thrown? #(-> [:guard [:success-leaf]] bt/hiccup->context))) 

(deftest repeat-no-count-test
  (is-thrown? #(-> [:repeat [:success-leaf]] bt/hiccup->context bt/tick)
              "Cannot loop endlessly")
  (is-thrown? #(-> [:repeat] bt/hiccup->context)
              "need at least one child")
  (is (= :running (-> [:repeat [:tick-eater {:count (constantly 1)}]] bt/hiccup->context
                      bt/tick bt/get-status)))
  (is (= 2 (-> [:repeat [:tick-eater {:count (constantly 1)}]] bt/hiccup->context
               bt/tick ctx/get-tick-count)))
  (is (= :running (-> [:repeat [:tick-eater {:count (constantly 1)}]] bt/hiccup->context
                      bt/tick bt/tick bt/get-status)))
  (is (= :running (-> [:repeat [:tick-eater {:count (constantly 1)}]] bt/hiccup->context
                      bt/tick bt/tick bt/tick bt/get-status)))
  (is (= :running (-> [:repeat {:while bt/bb-get}
                       [:consume-event {:event :bleh :wait? true}]]
                      bt/hiccup->context (bt/bb-set true) bt/tick bt/get-status)))
  (is (= :success (-> [:repeat {:while bt/bb-get}
                       [:consume-event {:event :bleh :wait? true}]]
                      bt/hiccup->context (bt/bb-set false) bt/tick bt/get-status))))

(deftest repeat-count-test
  (is (= :success (-> [:repeat {:count (constantly 5)} [:success-leaf]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :running (-> [:repeat {:while bt/bb-get
                                :count 25}
                       [:consume-event {:event :bleh :wait? true}]]
                      bt/hiccup->context (bt/bb-set true) bt/tick bt/get-status)))
  (is (= :success (-> [:repeat {:while bt/bb-get
                                :count 25}
                       [:consume-event {:event :bleh :wait? true}]]
                      bt/hiccup->context (bt/bb-set false) bt/tick bt/get-status))))


(deftest guard-selector-test
  (is-thrown? #(-> [:guard-selector [:success-leaf]] bt/hiccup->context))
  (is (-> [:guard-selector [:guard [:success-leaf] [:success-leaf]]]
          bt/hiccup->context))
  (is (= :running (-> [:guard-selector
                       [:guard [:success-leaf]
                        [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:guard-selector
                       [:guard [:success-leaf]
                        [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))
  (is (= :failure (-> [:guard-selector
                       [:guard [:failure-leaf]
                        [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :running (-> [:guard-selector
                       [:guard [:failure-leaf]
                        [:failure-leaf]]
                       [:guard [:success-leaf]
                        [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:guard-selector
                       [:guard [:failure-leaf]
                        [:failure-leaf]]
                       [:guard [:success-leaf]
                        [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick  bt/tick bt/get-status)))
  (is (= :running (-> [:guard-selector
                       [:guard [:failure-leaf]
                        [:failure-leaf]]
                       [:guard [:success-leaf]
                        [:inverter [:tick-eater {:count 1}]]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:guard-selector
                       [:guard [:failure-leaf]
                        [:failure-leaf]]
                       [:guard [:success-leaf]
                        [:inverter [:tick-eater {:count 1}]]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))

  ;; cancelling running nodes
  ;; we first test our counter
  (is (= :running (-> [:parallel {:policy :select}
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= 1 (-> [:parallel {:policy :select}
                [:sequence
                 [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                 [:repeat [:sequence
                           [:update {:func #(bt/bb-update % update :i inc)}]
                           [:tick-eater {:count 1}]]]]]
               bt/hiccup->context bt/tick bt/bb-get :i)))
  (is (= 2 (-> [:parallel {:policy :select}
                [:sequence
                 [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                 [:repeat [:sequence
                           [:update {:func #(bt/bb-update % update :i inc)}]
                           [:tick-eater {:count 1}]]]]]
               bt/hiccup->context bt/tick bt/tick bt/bb-get :i)))
  ;; we run and no cancel
  (is (= :running (-> [:parallel {:policy :select}
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector
                        [:guard [:predicate {:func #(<= (-> % bt/bb-get :i) 1)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(> (-> % bt/bb-get :i) 1)}]
                         [:success-leaf]]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :select}
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector
                        [:guard [:predicate {:func #(<= (-> % bt/bb-get :i) 1)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(> (-> % bt/bb-get :i) 1)}]
                         [:success-leaf]]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))
  (is (= :running (-> [:parallel {:policy :select}
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector
                        [:guard [:predicate {:func #(<= (-> % bt/bb-get :i) 1)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(> (-> % bt/bb-get :i) 1)}]
                         [:failure-leaf]]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))
  (is (= :running (-> [:parallel {:policy :select}
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector
                        [:guard [:predicate {:func #(<= (-> % bt/bb-get :i) 1)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(= (-> % bt/bb-get :i) 2)}]
                         [:tick-eater {:count 1}]]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :select} 
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector 
                        [:guard [:predicate {:func #(<= (-> % bt/bb-get :i) 1)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(= (-> % bt/bb-get :i) 2)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(> (-> % bt/bb-get :i) 2)}]
                         [:tick-eater {:count 1}]]]]
                      bt/hiccup->context bt/tick bt/tick bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :select}
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector
                        [:guard [:predicate {:func #(<= (-> % bt/bb-get :i) 1)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(= (-> % bt/bb-get :i) 2)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(> (-> % bt/bb-get :i) 2)}]
                         [:tick-eater {:count 1}]]]]
                      bt/hiccup->context bt/tick bt/tick bt/tick bt/tick bt/get-status)))
  (is (= :success (-> [:parallel {:policy :select} 
                       [:sequence
                        [:update {:func #(bt/bb-set % {:i 0 :result nil})}]
                        [:repeat [:sequence
                                  [:update {:func #(bt/bb-update % update :i inc)}]
                                  [:tick-eater {:count 1}]]]]
                       [:guard-selector
                        [:guard [:predicate {:func #(let [i (-> % bt/bb-get :i)]
                                                      (or (<=  1)
                                                          (= i 4)))}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(= (-> % bt/bb-get :i) 2)}]
                         [:tick-eater {:count 1}]]
                        [:guard [:predicate {:func #(= (-> % bt/bb-get :i) 3)}]
                         [:tick-eater {:count 1}]]]]
                      bt/hiccup->context bt/tick bt/tick bt/tick bt/tick bt/get-status))))

(deftest until-success-test
  (is (= :success (-> [:until-success [:success-leaf]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :running (-> [:until-success [:tick-eater {:count 1}]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:until-success [:tick-eater {:count 1}]] bt/hiccup->context bt/tick bt/tick bt/get-status))))

(deftest until-failure-test
  (is (= :success (-> [:until-failure [:failure-leaf]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :running (-> [:until-failure [:inverter [:tick-eater {:count 1}]]] bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:until-failure [:inverter [:tick-eater {:count 1}]]] bt/hiccup->context bt/tick bt/tick bt/get-status))))

(deftest timer-no-name-test
  (is (= :running (-> [:timer {:duration 5}] bt/hiccup->context (bt/tick 0) bt/get-status)))
  (is (= :running (-> [:timer {:duration 5}] bt/hiccup->context (bt/tick 0) (bt/tick 4) bt/get-status)))
  (is (= :success (-> [:timer {:duration 5}] bt/hiccup->context (bt/tick 0) (bt/tick 5) bt/get-status)))
  (is (= [[:a nil]] (-> [:sequence
                         [:timer {:duration 5}]
                         [:send-event {:event :a}]
                         [:timer {:duration 5}]
                         [:send-event {:event :b}]
                         [:timer {:duration 5}]
                         [:send-event {:event :c}]]
                        bt/hiccup->context (bt/tick 0) (bt/tick 15) bt/get-events))))

(deftest named-timer-test
  (is (= :running (-> [:timer {:timer :foo
                               :duration 5}]
                      bt/hiccup->context (bt/tick 0) bt/get-status)))
  (is (= :running (-> [:timer {:timer :foo
                               :duration 5}]
                      bt/hiccup->context (bt/tick 0) (bt/tick 4) bt/get-status)))
  (is (= :success (-> [:timer {:timer :foo
                               :duration 5}]
                      bt/hiccup->context (bt/tick 0) (bt/tick 5) bt/get-status)))
  (is (= [[:a nil]
          [:b nil]
          [:c nil]]
         (-> [:sequence
              [:timer {:timer :foo
                       :duration 5}]
              [:send-event {:event :a}]
              [:timer {:timer :foo
                       :duration 5}]
              [:send-event {:event :b}]
              [:timer {:timer :foo
                       :duration 5}]
              [:send-event {:event :c}]]
             bt/hiccup->context (bt/tick 0) (bt/tick 15) bt/get-events))))

(deftest always-success-test
  (is (= :success (-> [:always-success [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:always-success [:failure-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :running (-> [:always-success [:tick-eater {:count 1}]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:always-success [:tick-eater {:count 1}]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))
  (is (= :success (-> [:always-success [:inverter [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status))))

(deftest always-failure-test
  (is (= :failure (-> [:always-failure [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:always-failure [:failure-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :running (-> [:always-failure [:tick-eater {:count 1}]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:always-failure [:tick-eater {:count 1}]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status)))
  (is (= :failure (-> [:always-failure [:inverter [:tick-eater {:count 1}]]]
                      bt/hiccup->context bt/tick bt/tick bt/get-status))))

(deftest map-test
  (is (= :success (-> [:map {:seq [1 2 3] :bind-item :item}
                       [:success-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :failure (-> [:map {:seq [1 2 3] :bind-item :item}
                       [:failure-leaf]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= :success (-> [:sequence
                       [:update {:func (bt/bb-setter (list))}]
                       [:map {:seq [1 2 3] :bind-item :item}
                        [:update {:func #(bt/bb-update % conj (bt/get-var % :item))}]]]
                      bt/hiccup->context bt/tick bt/get-status)))
  (is (= [3 2 1] (-> [:sequence
                      [:update {:func (bt/bb-setter (list))}]
                      [:map {:seq [1 2 3] :bind-item :item}
                       [:update {:func #(bt/bb-update % conj (bt/get-var % :item))}]]]
                     bt/hiccup->context bt/tick bt/bb-get))))

(deftest on-cancel-test
  (is (= []
         (-> [:parallel {:policy :select}
              [:on-cancel [:send-event {:event :interrupted}]
               [:tick-eater {:count 2}]]
              [:sequence
               [:tick-eater {:count 1}]
               [:success-leaf]]]
             bt/hiccup->context bt/tick bt/get-events)))
  (is (= [[:interrupted nil]]
         (-> [:parallel {:policy :select}
              [:on-cancel [:send-event {:event :interrupted}]
               [:tick-eater {:count 2}]]
              [:sequence
               [:tick-eater {:count 1}]
               [:success-leaf]]]
             bt/hiccup->context bt/tick bt/tick bt/get-events)))
  (is (= [[:interrupted nil]]
         (-> [:parallel {:policy :select}
              [:on-cancel [:send-event {:event :interrupted}]
               [:tick-eater {:count 2}]]
              [:success-leaf]]
             bt/hiccup->context bt/tick bt/get-events)))) 

;; (deftest hiccup-func-call-test
;;   (is (= [:inverter [:success-leaf]] (h/prepare [:inverter [:success-leaf]])))
;;   (is (= [:inverter [:success-leaf]] (h/prepare [:inverter [(fn [] [:success-leaf])]])))
;;   (is (= [:sequence [:success-leaf] [:success-leaf]]
;;          (h/prepare [:sequence [:<> [:success-leaf] [:success-leaf]]])))
;;   (is (= [:sequence [:success-leaf] [:success-leaf]]
;;          (h/prepare [:sequence [(fn [] [:<> [:success-leaf] [:success-leaf]])]])))
;;   (is (= [:sequence [:success-leaf] [:success-leaf]]
;;          (h/prepare [:sequence [(fn [n] (into [:<>] (repeat n [:success-leaf]))) 2]])))
;;   (is (= [:inverter [:success-leaf]] (h/prepare [:inverter [(fn [] [(fn [] [:success-leaf])])]])))
;;   (is-thrown? #(h/prepare [:<> [:success-leaf]]))

;;   (is (= :failure (-> [:inverter [:success-leaf]] bt/hiccup->context bt/tick bt/get-status)))
;;   (is (= :failure (-> [:inverter [(fn [_] [:success-leaf]) 12]] bt/hiccup->context bt/tick bt/get-status)))
;;   (is (= :success (-> [:sequence [(fn [] [:<> [:success-leaf] [:success-leaf]])]] bt/hiccup->context bt/tick bt/get-status)))
;;   (is (= 3 (-> [:sequence [(fn [] [:<> [:success-leaf] [:success-leaf]])]] bt/hiccup->context bt/tick ctx/get-tick-count))))
