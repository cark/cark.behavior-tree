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
                                                   :approvals-received #{}
                                                   :stakeholders #{})}])

(def receive-approval [:on-event {:event :approval-received :wait? true :bind-arg :approval-from
                                  :pick? #(= (bt/get-var %1 :department) %2)}
                       [:update {:func #(bt/bb-update-in % [:approvals-received] conj (bt/get-var % :approval-from))}]])

(deftest receive-approval-test
  (let [ctx (-> [:sequence init-approvals
                 [:bind {:let [:department :legal]}
                  receive-approval]]
                bt/hiccup->context bt/tick)]
    (is (= :running (-> ctx bt/get-status)))
    (is (= #{} (-> ctx (bt/bb-get-in [:approvals-received]))))
    (is (= :success (-> ctx (bt/send-event :approval-received :legal) bt/tick bt/get-status)))
    (is (= #{:legal} (-> ctx (bt/send-event :approval-received :legal) bt/tick (bt/bb-get-in [:approvals-received]))))
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
                                [:predicate {:func #(bt/bb-get-in % [:approvals-received (bt/get-var % :department)])}]
                                ;;or in the process of being approved
                                monitor-department-mails])

(deftest monitor-stakeholder-mails-test
  (let [ctx (-> [:bind {:let [:department :legal]}
                 monitor-stakeholder-mails]
                bt/hiccup->context)]
    ;; not selected
    (is (= :success (-> ctx bt/tick bt/get-status)))
    ;; selected and approved
    (is (= :success (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals-received #{:legal}}) bt/tick bt/get-status)))
    ;; selected no approval
    (is (= :running (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals-received #{}}) bt/tick bt/get-status)))))

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
                                                                              (bt/bb-get-in % [:approvals-received])))
                                                       0)}]]]])

(deftest monitor-all-departments-test
  (let [ctx (-> monitor-all-departments bt/hiccup->context)]
    (is (= :running (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals-received #{}})
                        bt/tick bt/get-status)))
    (is (= :success (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals-received #{}})
                        (bt/send-event :approval-received :legal) bt/tick bt/get-status)))
    (is (= :running (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals-received #{}})
                        bt/tick (bt/send-event :add-stakeholder :production) bt/tick
                        (bt/send-event :approval-received :legal) bt/tick bt/get-status)))
    (is (= :success (-> ctx (bt/bb-set {:stakeholders #{:legal} :approvals-received #{}})
                        bt/tick (bt/send-event :add-stakeholder :production) bt/tick
                        (bt/send-event :approval-received :legal) bt/tick
                        (bt/send-event :approval-received :production) bt/tick bt/get-status))) ))
