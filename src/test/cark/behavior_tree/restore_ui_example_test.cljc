(ns cark.behavior-tree.restore-ui-example-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.state-machine :as sm]
            [cark.behavior-tree.context :as ctx]))

;; we define a backup restore page
;;
;; - a button "restore" will start the process
;; - first a confirmation dialog will open
;; - restore button is grayed out
;; - the dialog cancel button makes the confirm dialog disapear
;; - the dialog's ok button will start the open file dialog
;; - html5 only gives a result when the user selects a file, not when he
;; cancels the open file dialog,we need to work around this
;; - when we have the file data, we close the confirmation dialog and send a restore event
;; - we open a "restoring" dialog
;; - we receive a result that might be
;; - success : open a success dialog having an ok button, activate restore button
;; - failure : open a failure dialog having an ok button, activate restore button
;;
;; This would usually be developped bottom up, step by step, with testing along the way
;; but i felt adventurous on this one, and it was harder than it should have been.
;; Better workflow can be seen in the change control example.

(deftest restore-test
  (let [end-dialog (fn [result dialog]
                     [:always-failure
                      [:guard [:predicate {:func #(= (bt/get-var % :result) result)}]
                       [:sequence
                        [:update {:func (bt/bb-updater assoc dialog true)}]
                        [:on-event {:event :ok-pressed :wait? true}
                         [:update {:func (bt/bb-updater assoc dialog false)}]]]]])
        ctx (-> [:repeat
                 [:sequence
                  [:update {:func (bt/bb-updater assoc :restore-button true)}]
                  [:on-event {:event :restore-pressed :wait? true}
                   [:repeat
                    [:sequence
                     [:update {:func (bt/bb-updater assoc :restore-button false :confirm-dialog true)}]
                     [:parallel {:policy {:success :some :failure :some}}
                      ;; no confirmation
                      [:on-event {:event :cancel-pressed :wait? true}
                       [:sequence
                        [:update {:func (bt/bb-updater assoc :confirm-dialog false :restore-button true)}]
                        [:consume-event {:event :restore-pressed :wait? true}]
                        [:failure-leaf]]]
                      ;; html5 workaround loop
                      [:until-success 
                       [:on-event {:event :confirm-pressed :wait? true}
                        [:sequence
                         [:send-event {:event :open-file-dialog}]
                         [:on-event {:event :got-file-data :bind-arg :file-data :wait? true}
                          [:send-event {:event :restore-file :arg (bt/var-getter :file-data)}]]
                         [:update {:func (bt/bb-updater assoc :confirm-dialog false :restoring-dialog true)}]]]]]
                     ;; manage restore result then loop
                     [:on-event {:event :restore-result :bind-arg :result :wait? true}
                      [:sequence                       
                       [:update {:func (bt/bb-updater assoc :restore-button true :restoring-dialog false)}]
                       [:parallel {:policy :select}
                        [end-dialog :success :success-dialog]
                        [end-dialog :error :error-dialog]
                        [:on-event {:event :restore-pressed :wait? true}
                         [:update {:func (bt/bb-updater assoc :success-dialog false :error-dialog false)}]]]]]]]]]]
                bt/hiccup->context bt/tick)
        send-event (fn send-event
                     ([ctx event]
                      (send-event ctx event nil))
                     ([ctx event arg]
                      (-> (bt/send-event ctx event arg) bt/tick)))]
    (is ctx)
    (is (-> ctx (bt/bb-get-in [:restore-button])))
    (is (= :running (-> ctx (send-event :restore-pressed) bt/get-status)))
    (is (not (-> ctx (send-event :restore-pressed) (bt/bb-get-in [:restore-button]))))
    (is (-> ctx (send-event :restore-pressed) (bt/bb-get-in [:confirm-dialog])))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :cancel-pressed) (bt/bb-get-in [:confirm-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :cancel-pressed) (bt/bb-get-in [:restore-button])))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :cancel-pressed) (send-event :restore-pressed)
                 (bt/bb-get-in [:restore-button]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :cancel-pressed) (send-event :restore-pressed)
            (bt/bb-get-in [:confirm-dialog])))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (bt/bb-get-in [:confirm-dialog])))
    (is (= [[:open-file-dialog nil]] (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) bt/get-events)))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :cancel-pressed)
                 (bt/bb-get-in [:confirm-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :cancel-pressed)
            (bt/bb-get-in [:restore-button])))
    (is (= [[:restore-file :some-data]]
           (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
               bt/get-events)))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (bt/bb-get-in [:confirm-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
            (bt/bb-get-in [:restoring-dialog])))
    
    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :success) (bt/bb-get-in [:restoring-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
            (send-event :restore-result :success) (bt/bb-get-in [:success-dialog])))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :success) (bt/bb-get-in [:error-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
            (send-event :restore-result :success) (bt/bb-get-in [:restore-button])))

    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :error) (bt/bb-get-in [:restoring-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
            (send-event :restore-result :error) (bt/bb-get-in [:error-dialog])))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :error) (bt/bb-get-in [:success-dialog]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
            (send-event :restore-result :success) (bt/bb-get-in [:restore-button])))

    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :success) (send-event :ok-pressed) (bt/bb-get-in [:success-dialog]))))
    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :error) (send-event :ok-pressed) (bt/bb-get-in [:error-dialog]))))

    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :error) (send-event :restore-pressed) (bt/bb-get-in [:restore-button]))))
    (is (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
            (send-event :restore-result :error) (send-event :restore-pressed) (bt/bb-get-in [:confirm-dialog])))

    (is (not (-> ctx (send-event :restore-pressed) (send-event :confirm-pressed) (send-event :got-file-data :some-data)
                 (send-event :restore-result :error) (send-event :restore-pressed) (bt/bb-get-in [:error-dialog]))))))

