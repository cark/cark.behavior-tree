(ns cark.behavior-tree.change-control-example-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.state-machine :as sm]
            [cark.behavior-tree.context :as ctx]
            [clojure.set :as set]))

;; In this file we describe a workflow to come up with a
;; behavior tree while testing it

(defn log [value]
  (tap> value)
  value)

(defn is-thrown?
  ([func]
   (is (thrown? #?(:cljs js/Object :clj Exception) (func))))
  ([func text]
   (is (thrown? #?(:cljs js/Object :clj Exception) (func))
       text)))

;; We model a simplified change control process in the pharma industry
;;
;; The change proposal is a document with the situation as-is, to-be
;; and regulatory impact
;;
;; A change proposal must be first sent to all stakeholder departments.
;; We then wait for their answer, possibly sending email reminders
;;
;; some email responses might require us to add a stakeholder
;; which we then email, waiting in turn for their responses
;;
;; once every stakeholder approves the change, we're ready to go to the
;; change control committee.
;;
;; this committee might accept, reject or put the change on hold
;; accept and reject are our stop points
;; if the change is on hold, we will start the whole processus anew by amending the
;; change proposal document and sending it again to all stakeholders 
;;

(def sec 1000)
(def minute (* 60 sec))
(def hour (* 60 minute))
(def day (* 24 hour))
(def week (* 7 day))
(def month (* 30 day))

(def departments #{:production :quality-ensurance :quality-control :regulatory-affairs :legal})
(def document-parts #{:as-is :to-be :regulatory-impact})

(def mails [{:name :mail :wait 0}
            {:name :reminder :wait week}
            {:name :reminder :wait week}
            {:name :urgent-reminder :wait (* 2 day)}
            {:name :last-reminder :wait (* 2 day)}
            {:name :consider-accepted :wait day}])

(def send-mail [:sequence
                [:timer {:timer (bt/var-getter :department) ;; we need named timers for each department
                         :duration (bt/var-getter :mail-wait)}]
                [:send-event {:event :send-mail :arg #(vector (bt/get-var % :mail-name) (bt/get-var % :department))}]])

;; we start with emails and test that

(deftest send-mail-test
  (is (-> send-mail bt/hiccup->context))
  (let [ctx (-> [:bind {:let [:department :legal ;;we're using these bound variables to send emails messages
                              :mail-name :mail
                              :mail-wait 5]}
                 send-mail] bt/hiccup->context (bt/tick 0))]
    (is (= :running (-> ctx bt/get-status)))
    (is (= [] (-> ctx bt/get-events)))
    (is (= :running (-> ctx (bt/tick+ 4) bt/get-status)))
    (is (= :success (-> ctx (bt/tick+ 5) bt/get-status)))
    (is (= [[:send-mail [:mail :legal]]] (-> ctx (bt/tick+ 5) bt/get-events)))))

;; we want to send email reminders and finally succeed if we receive no answer

(def send-mails [:map {:seq mails :bind-item :mail-def}
                 [:bind {:let [:mail-name #(:name (bt/get-var % :mail-def))
                               :mail-wait #(:wait (bt/get-var % :mail-def))]}
                  send-mail]])

(deftest send-mails-test
  (let [ctx (-> [:bind {:let [:department :legal]}
                 send-mails]
                bt/hiccup->context (bt/tick 0))]
    (is (= :running (-> ctx bt/get-status)))
    (is (= [[:send-mail [:mail :legal]]] (-> ctx bt/get-events)))
    (is (= [] (-> ctx (bt/tick+ day) bt/get-events)))
    (is (= [[:send-mail [:reminder :legal]]] (-> ctx (bt/tick+ week) bt/get-events)))
    (is (= [[:send-mail [:reminder :legal]]] (-> ctx (bt/tick+ week) (bt/tick+ week) bt/get-events)))
    (is (= [[:send-mail [:urgent-reminder :legal]]]
           (-> ctx (bt/tick+ week) (bt/tick+ week) (bt/tick+ (* 2 day)) bt/get-events)))
    (is (= [[:send-mail [:last-reminder :legal]]]
           (-> ctx (bt/tick+ week) (bt/tick+ week) (bt/tick+ (* 2 day)) (bt/tick+ (* 2 day)) bt/get-events)))
    (is (= [[:send-mail [:consider-accepted :legal]]]
           (-> ctx (bt/tick+ week) (bt/tick+ week) (bt/tick+ (* 2 day)) (bt/tick+ (* 2 day)) (bt/tick+ day) bt/get-events)))
    (is (= :success
           (-> ctx (bt/tick+ week) (bt/tick+ week) (bt/tick+ (* 2 day)) (bt/tick+ (* 2 day)) (bt/tick+ day) bt/get-status)))
    (is (= [[:send-mail [:reminder :legal]]
            [:send-mail [:reminder :legal]]
            [:send-mail [:urgent-reminder :legal]]
            [:send-mail [:last-reminder :legal]]
            [:send-mail [:consider-accepted :legal]]]
           (-> ctx (bt/tick+ (+ (* 2 week) (* 4 day) day)) bt/get-events )))))

;; we'll store received email approvals in a set in the blackboard, along with the concerned stakeholders

(def init-approvals [:update {:func (bt/bb-updater assoc
                                                   :approvals #{}
                                                   :stakeholders #{})}])

(def receive-approval [:on-event {:event :approval-received :wait? true :bind-arg :approval-from
                                  :pick? #(= (bt/get-var %1 :department) %2)}
                       [:update {:func #(bt/bb-update-in % [:approvals] conj (bt/get-var % :approval-from))}]])

(deftest receive-approval-test
  (let [ctx (-> [:sequence init-approvals
                 [:bind {:let [:department :legal]}
                  receive-approval]]
                bt/hiccup->context bt/tick)]
    (is (= :running (-> ctx bt/get-status)))
    (is (= #{} (-> ctx (bt/bb-get-in [:approvals]))))
    (is (= :success (-> ctx (bt/send-event :approval-received :legal) bt/tick bt/get-status)))
    (is (= #{:legal} (-> ctx (bt/send-event :approval-received :legal) bt/tick (bt/bb-get-in [:approvals]))))
    (is-thrown? #(-> ctx (bt/send-event :approval-received :production) bt/tick bt/get-status))))

;; once we receive an approval, we should stop sending mail reminders

(def monitor-department-mails [:parallel {:policy :select}
                               receive-approval
                               send-mails])

(deftest monitor-department-mails-test
  (let [ctx (-> [:sequence init-approvals
                 [:bind {:let [:department :legal]}
                  monitor-department-mails]]
                bt/hiccup->context (bt/tick 0))]
    (is (= [[:send-mail [:mail :legal]]] (-> ctx bt/get-events)))
    (is (= [[:send-mail [:reminder :legal]]] (-> ctx (bt/tick+ week) bt/get-events)))
    (is (= [] (-> ctx (bt/send-event :approval-received :legal)
                  (bt/tick+ 0) (bt/tick+ week) bt/get-events)))
    (is (= :success (-> ctx (bt/send-event :approval-received :legal)
                        (bt/tick+ 0) (bt/tick+ week) bt/get-status)))
    (is (= :success (-> ctx (bt/send-event :approval-received :legal) (bt/tick+ 0) bt/get-status)))))

;; provide a way to add stakeholders

(def add-stakeholder [:on-event {:event :add-stakeholder :bind-arg :stakeholder :wait? true}
                      [:update {:func #(bt/bb-update-in % [:stakeholders] conj (bt/get-var % :stakeholder))}]])

(def add-stakeholders [:repeat add-stakeholder])

(deftest add-stakeholders-test
  (let [ctx (-> [:sequence init-approvals add-stakeholders]
                bt/hiccup->context bt/tick)]
    (is (= :running (-> ctx bt/get-status)))
    (is (= #{} (-> ctx (bt/bb-get-in [:stakeholders]))))
    (is (= #{:legal} (-> ctx (bt/send-event :add-stakeholder :legal) bt/tick (bt/bb-get-in [:stakeholders]))))
    (is (= #{:legal :production}
           (-> ctx (bt/send-event :add-stakeholder :legal) (bt/send-event :add-stakeholder :production)
               bt/tick (bt/bb-get-in [:stakeholders]))))))

;; a department should check if it was added to the stakeholders if it wasn't already

(def monitor-stakeholder-mails [:select
                                ;; either we're not selected
                                [:predicate {:func #(not (bt/bb-get-in % [:stakeholders (bt/get-var % :department)]))}]
                                ;; or already approved
                                [:predicate {:func #(bt/bb-get-in % [:approvals (bt/get-var % :department)])}]
                                ;; or in the process of being approved
                                monitor-department-mails])

(deftest monitor-stakeholder-mails-test
  (let [ctx (-> [:bind {:let [:department :legal]}
                 monitor-stakeholder-mails]
                bt/hiccup->context)]
    ;; not selected
    (is (= :success (-> ctx bt/tick bt/get-status)))
    ;; selected and approved
    (is (= :success (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals #{:legal}}) bt/tick bt/get-status)))
    ;; selected no approval
    (is (= :running (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals #{}}) bt/tick bt/get-status)))))

;; check all departments in a loop, until no more stakeholder needs approval, adding new stakeholders as needed

(def monitor-all-departments [:parallel {:policy :select}
                              add-stakeholders
                              [:until-success
                               [:sequence
                                (into [:parallel {:policy :sequence :rerun-children true}]
                                      (map (fn [department]
                                             [:bind {:let [:department department]} monitor-stakeholder-mails])
                                           departments))
                                [:predicate {:func #(= (count (set/difference (bt/bb-get-in % [:stakeholders])
                                                                              (bt/bb-get-in % [:approvals])))
                                                       0)}]]]])

(deftest monitor-all-departments-test
  (let [ctx (-> monitor-all-departments bt/hiccup->context)]
    (is (= :running (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals #{}})
                        bt/tick bt/get-status)))
    (is (= :success (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals #{}})
                        (bt/send-event :approval-received :legal) bt/tick bt/get-status)))
    (is (= :running (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals #{}})
                        bt/tick (bt/send-event :add-stakeholder :production) bt/tick
                        (bt/send-event :approval-received :legal) bt/tick bt/get-status)))
    (is (= :success (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals #{}})
                        bt/tick (bt/send-event :add-stakeholder :production) bt/tick
                        (bt/send-event :approval-received :legal) bt/tick
                        (bt/send-event :approval-received :production) bt/tick bt/get-status))) ))

;; we need at least one stakeholder, or the whole email waiting will be skipped !

(def ensure-stakeholder [:select
                         [:predicate {:func #(seq (bt/bb-get-in % [:stakeholders]))}]
                         [:sequence
                          [:send-event {:event :please-add-stakeholders}]
                          add-stakeholder]])

(deftest ensure-stakeholder-test
  (let [ctx (bt/hiccup->context ensure-stakeholder)]
    (is (= [[:please-add-stakeholders nil]]
           (-> ctx (bt/bb-update assoc :stakeholders #{}) bt/tick bt/get-events)))
    (is (= :running (-> ctx (bt/bb-update assoc :stakeholders #{}) bt/tick bt/get-status)))
    (is (= :success (-> ctx (bt/bb-update assoc :stakeholders #{}) bt/tick
                        (bt/send-event :add-stakeholder :legal) bt/tick
                        bt/get-status)))
    (is (= #{:legal} (-> ctx (bt/bb-update assoc :stakeholders #{}) bt/tick
                         (bt/send-event :add-stakeholder :legal) bt/tick
                         (bt/bb-get-in [:stakeholders])))) 
    (is (= [] (-> ctx (bt/bb-update assoc :stakeholders #{:legal}) bt/tick bt/get-events)))
    (is (= :success (-> ctx (bt/bb-update assoc :stakeholders #{:legal}) bt/tick bt/get-status)))))

;; we're done with approvals, let's do the document now
;; we need a way to edit the document parts

(defn edit-document-part [part]
  [:on-event {:event :edit-document :pick? (fn [ctx [arg-part arg-data]] (= part arg-part))
              :wait? true :bind-arg :edit-arg}
   [:update {:func #(bt/bb-update % assoc-in [:document part] (second (bt/get-var % :edit-arg)))}]])

(deftest edit-document-part-test
  (let [ctx (-> (edit-document-part :as-is) bt/hiccup->context)]
    (-> (= :success (-> ctx (bt/send-event :edit-document [:as-is :some-data]) bt/tick bt/get-status)))
    (-> (= :some-data (-> ctx (bt/send-event :edit-document [:as-is :some-data]) bt/tick (bt/bb-get-in [:document :as-is]))))
    (is-thrown? #(-> ctx (bt/send-event :edit-document [:wrong-part :some-data]) bt/tick bt/get-status))))

;; there are several parts to a document

(def edit-document-parts (into [:parallel {:policy :sequence :rerun-children true}]
                               (map edit-document-part document-parts)))

(deftest edit-document-parts-test
  (let [ctx (-> edit-document-parts bt/hiccup->context)]
    (is (= {:as-is :blah} (-> ctx (bt/send-event :edit-document [:as-is :blah]) bt/tick (bt/bb-get-in [:document]))))
    (is (= {:as-is :foo} (-> ctx
                             (bt/send-event :edit-document [:as-is :blah]) bt/tick
                             (bt/send-event :edit-document [:as-is :foo]) bt/tick
                             (bt/bb-get-in [:document]))))
    (is (= {:as-is :foo
            :to-be :bar
            :regulatory-impact :bam}
           (-> ctx
               (bt/send-event :edit-document [:as-is :foo]) bt/tick
               (bt/send-event :edit-document [:to-be :bar]) bt/tick
               (bt/send-event :edit-document [:regulatory-impact :bam]) bt/tick
               (bt/bb-get-in [:document]))))

    (is (= :running
           (-> ctx
               (bt/send-event :edit-document [:as-is :foo]) bt/tick
               (bt/send-event :edit-document [:to-be :bar]) bt/tick
               (bt/send-event :edit-document [:regulatory-impact :bam]) bt/tick
               bt/get-status)))))

;; once all three parts of the document are filled, we may seal the document, and continue

(def sealable-document [:parallel {:policy :select}
                        edit-document-parts ;; this one will never fail or succeed
                        [:until-success
                         [:on-event {:event :seal-document :wait? true}
                          [:select
                           [:guard [:predicate {:func #(>= (count (->> (bt/bb-get-in % [:document])
                                                                       (map second)
                                                                       (filter identity)))
                                                           3)}]
                            [:success-leaf]]
                           [:sequence
                            [:send-event {:event :cannot-seal :arg (constantly :incomplete-document)}]
                            [:failure-leaf]]]]]])

(deftest sealable-document-test
  (let [ctx (-> sealable-document bt/hiccup->context)]
    (is (= :running (-> ctx
                        (bt/send-event :edit-document [:regulatory-impact :bam]) bt/tick
                        bt/get-status)))
    (is (= [[:cannot-seal :incomplete-document]]
           (-> ctx
               (bt/send-event :edit-document [:regulatory-impact :bam]) bt/tick
               (bt/send-event :seal-document) bt/tick
               bt/get-events)))
    (is (= :success
           (-> ctx
               (bt/send-event :edit-document [:regulatory-impact :bam]) bt/tick
               (bt/send-event :seal-document) bt/tick
               (bt/send-event :edit-document [:as-is :foo]) bt/tick
               (bt/send-event :edit-document [:to-be :bar]) bt/tick
               (bt/send-event :seal-document) bt/tick
               bt/get-status)))
    (is (= {:regulatory-impact :bam
            :as-is :foo
            :to-be :bar}
           (-> ctx
               (bt/send-event :edit-document [:regulatory-impact :bam]) bt/tick
               (bt/send-event :seal-document) bt/tick
               (bt/send-event :edit-document [:as-is :foo]) bt/tick
               (bt/send-event :edit-document [:to-be :bar]) bt/tick
               (bt/send-event :seal-document) bt/tick
               (bt/bb-get-in [:document]))))))

;; we now model the committee interaction

(def committee [:sequence
                [:send-event {:event :go-to-committee}]
                [:on-event {:event :committee-result :bind-arg :result :wait? true}
                 [:select
                  [:guard [:predicate {:func #(= :accept (bt/get-var % :result))}]
                   [:send-event {:event :done :arg (constantly :accept)}]]
                  [:guard [:predicate {:func #(= :reject (bt/get-var % :result))}]
                   [:send-event {:event :done :arg (constantly :reject)}]]
                  [:guard [:predicate {:func #(= :on-hold (bt/get-var % :result))}]
                   [:sequence
                    [:send-event {:event :continue :arg (constantly :on-hold)}]
                    [:failure-leaf]]]]]])

(deftest comittee-test
  (let [ctx (bt/hiccup->context committee)]
    (is (= [[:go-to-committee nil]] (-> ctx bt/tick bt/get-events)))
    (is (= :success (-> ctx (bt/send-event :committee-result :accept) bt/tick bt/get-status)))
    (is (= [[:done :accept]] (-> ctx bt/tick (bt/send-event :committee-result :accept) bt/tick bt/get-events)))
    (is (= :success (-> ctx (bt/send-event :committee-result :reject) bt/tick bt/get-status)))
    (is (= [[:done :reject]] (-> ctx bt/tick (bt/send-event :committee-result :reject) bt/tick bt/get-events)))
    (is (= :failure (-> ctx (bt/send-event :committee-result :on-hold) bt/tick bt/get-status)))
    (is (= [[:continue :on-hold]] (-> ctx bt/tick (bt/send-event :committee-result :on-hold) bt/tick bt/get-events)))))

;; we amend the document and notify all stakeholders when committee responds with on-hold

(def change-control [:sequence
                     init-approvals
                     [:until-success
                      [:sequence
                       [:send-event {:event :please-edit-document}]
                       sealable-document
                       ensure-stakeholder
                       monitor-all-departments
                       [:select
                        committee
                        [:sequence
                         [:update {:func (bt/bb-assocer-in [:approvals] #{})}]
                         [:failure-leaf]]]]]])


(deftest change-control-test
  (let [ctx (-> (bt/hiccup->context change-control) bt/tick)]
    (is (= [[:please-edit-document nil]] (-> ctx bt/get-events)))
    (let [ctx (-> ctx
                  (bt/send-event :edit-document [:as-is :it-s-fiiine]) bt/tick
                  (bt/send-event :edit-document [:to-be :paradise]) bt/tick
                  (bt/send-event :edit-document [:regulatory-impact :klabango!]) bt/tick
                  (bt/send-event :seal-document) bt/tick)]
      (is (= {:as-is :it-s-fiiine
              :to-be :paradise
              :regulatory-impact :klabango!} (-> ctx (bt/bb-get-in [:document]))))
      (is (= [[:please-add-stakeholders nil]] (-> ctx bt/get-events)))
      (is (= [[:send-mail [:mail :legal]]] (-> ctx (bt/send-event :add-stakeholder :legal) bt/tick bt/get-events)))
      (is (= [[:send-mail [:mail :production]]] (-> ctx
                                                    (bt/send-event :add-stakeholder :legal) bt/tick
                                                    (bt/send-event :add-stakeholder :production) bt/tick
                                                    bt/get-events)))
      (is (= #{[:send-mail [:reminder :production]]
               [:send-mail [:reminder :legal]]}
             (-> ctx
                 (bt/send-event :add-stakeholder :legal) bt/tick
                 (bt/send-event :add-stakeholder :production) bt/tick
                 (bt/tick+ week)
                 bt/get-events set)))
      (let [ctx (-> ctx
                    (bt/send-event :add-stakeholder :legal) bt/tick
                    (bt/send-event :add-stakeholder :production) bt/tick
                    (bt/send-event :approval-received :legal) bt/tick
                    (bt/send-event :approval-received :production) bt/tick)]
        (is (= [[:go-to-committee nil]] (-> ctx bt/get-events)))
        (is (= [[:done :accept]] (-> ctx (bt/send-event :committee-result :accept) bt/tick bt/get-events)))
        (is (= :success (-> ctx (bt/send-event :committee-result :accept) bt/tick bt/get-status)))
        (is (= [[:done :reject]] (-> ctx (bt/send-event :committee-result :reject) bt/tick bt/get-events)))
        (is (= :success (-> ctx (bt/send-event :committee-result :reject) bt/tick bt/get-status)))
        (is (= [[:continue :on-hold]
                [:please-edit-document nil]] (-> ctx (bt/send-event :committee-result :on-hold) bt/tick bt/get-events)))
        (is (= :running (-> ctx (bt/send-event :committee-result :running) bt/tick bt/get-status)))
        (let [ctx (-> ctx (bt/send-event :committee-result :on-hold) bt/tick)]
          (is (= #{[:send-mail [:mail :production]]
                   [:send-mail [:mail :legal]]}
                 (-> ctx (bt/send-event :seal-document) bt/tick bt/get-events set))))))))

;; alright we're done, let's see how big the definition of our change control tree is

(comment
  (def sec 1000)
  (def minute (* 60 sec))
  (def hour (* 60 minute))
  (def day (* 24 hour))
  (def week (* 7 day))
  (def month (* 30 day))

  (def departments #{:production :quality-ensurance :quality-control :regulatory-affairs :legal})
  (def document-parts #{:as-is :to-be :regulatory-impact})

  (def mails [{:name :mail :wait 0}
              {:name :reminder :wait week}
              {:name :reminder :wait week}
              {:name :urgent-reminder :wait (* 2 day)}
              {:name :last-reminder :wait (* 2 day)}
              {:name :consider-accepted :wait day}])

  (def send-mail [:sequence
                  [:timer {:timer (bt/var-getter :department) ;; we need named timers for each department
                           :duration (bt/var-getter :mail-wait)}]
                  [:send-event {:event :send-mail :arg #(vector (bt/get-var % :mail-name) (bt/get-var % :department))}]])

  (def send-mails [:map {:seq mails :bind-item :mail-def}
                   [:bind {:let [:mail-name #(:name (bt/get-var % :mail-def))
                                 :mail-wait #(:wait (bt/get-var % :mail-def))]}
                    send-mail]])

  (def init-approvals [:update {:func (bt/bb-updater assoc
                                                     :approvals #{}
                                                     :stakeholders #{})}])

  (def receive-approval [:on-event {:event :approval-received :wait? true :bind-arg :approval-from
                                    :pick? #(= (bt/get-var %1 :department) %2)}
                         [:update {:func #(bt/bb-update-in % [:approvals] conj (bt/get-var % :approval-from))}]])

  (def monitor-department-mails [:parallel {:policy :select}
                                 receive-approval
                                 send-mails])

  (def add-stakeholder [:on-event {:event :add-stakeholder :bind-arg :stakeholder :wait? true}
                        [:update {:func #(bt/bb-update-in % [:stakeholders] conj (bt/get-var % :stakeholder))}]])

  (def add-stakeholders [:repeat add-stakeholder])

  (def monitor-stakeholder-mails [:select
                                  ;; either we're not selected
                                  [:predicate {:func #(not (bt/bb-get-in % [:stakeholders (bt/get-var % :department)]))}]
                                  ;; or already approved
                                  [:predicate {:func #(bt/bb-get-in % [:approvals (bt/get-var % :department)])}]
                                  ;; or in the process of being approved
                                  monitor-department-mails])

  (def monitor-all-departments [:parallel {:policy :select}
                                add-stakeholders
                                [:until-success
                                 [:sequence
                                  (into [:parallel {:policy :sequence :rerun-children true}]
                                        (map (fn [department]
                                               [:bind {:let [:department department]} monitor-stakeholder-mails])
                                             departments))
                                  [:predicate {:func #(= (count (set/difference (bt/bb-get-in % [:stakeholders])
                                                                                (bt/bb-get-in % [:approvals])))
                                                         0)}]]]])

  (def ensure-stakeholder [:select
                           [:predicate {:func #(seq (bt/bb-get-in % [:stakeholders]))}]
                           [:sequence
                            [:send-event {:event :please-add-stakeholders}]
                            add-stakeholder]])

  (defn edit-document-part [part]
    [:on-event {:event :edit-document :pick? (fn [ctx [arg-part arg-data]] (= part arg-part))
                :wait? true :bind-arg :edit-arg}
     [:update {:func #(bt/bb-update % assoc-in [:document part] (second (bt/get-var % :edit-arg)))}]])

  (def edit-document-parts (into [:parallel {:policy :sequence :rerun-children true}]
                                 (map edit-document-part document-parts)))

  (def sealable-document [:parallel {:policy :select}
                          edit-document-parts ;; this one will never fail or succeed
                          [:until-success
                           [:on-event {:event :seal-document :wait? true}
                            [:select
                             [:guard [:predicate {:func #(>= (count (->> (bt/bb-get-in % [:document])
                                                                         (map second)
                                                                         (filter identity)))
                                                             3)}]
                              [:success-leaf]]
                             [:sequence
                              [:send-event {:event :cannot-seal :arg (constantly :incomplete-document)}]
                              [:failure-leaf]]]]]])

  (def committee [:sequence
                  [:send-event {:event :go-to-committee}]
                  [:on-event {:event :committee-result :bind-arg :result :wait? true}
                   [:select
                    [:guard [:predicate {:func #(= :accept (bt/get-var % :result))}]
                     [:send-event {:event :done :arg (constantly :accept)}]]
                    [:guard [:predicate {:func #(= :reject (bt/get-var % :result))}]
                     [:send-event {:event :done :arg (constantly :reject)}]]
                    [:guard [:predicate {:func #(= :on-hold (bt/get-var % :result))}]
                     [:sequence
                      [:send-event {:event :continue :arg (constantly :on-hold)}]
                      [:failure-leaf]]]]]])

  (def change-control [:sequence
                       init-approvals
                       [:until-success
                        [:sequence
                         [:send-event {:event :please-edit-document}]
                         sealable-document
                         ensure-stakeholder
                         monitor-all-departments
                         [:select
                          committee
                          [:sequence
                           [:update {:func (bt/bb-assocer-in [:approvals] #{})}]
                           [:failure-leaf]]]]]])
  )
