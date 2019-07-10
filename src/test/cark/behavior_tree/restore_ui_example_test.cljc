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
;; This would usually be developped bottom up, step by step, with testing along the way,
;; but i felt adventurous on this one.

(deftest restore-test
  (let [ctx (-> [:sequence
                 ;;init
                 [:update {:func (bt/bb-updater assoc
                                                :restore-button true
                                                :confirm-dialog false
                                                :restoring-dialog false
                                                :success-dialog false
                                                :error-dialog false)}]
                 ;;keep running
                 [:parallel {:policy {:success :every :failure :every} :rerun-children true}
                  ;; the start point, checking the restore button
                  [:guard [:predicate {:func (bt/bb-getter-in [:restore-button])}]
                   [:on-event {:event :restore-pressed :wait? true}
                    [:update {:func (bt/bb-updater assoc
                                                   :restore-button false
                                                   :confirm-dialog true
                                                   :success-dialog false
                                                   :error-dialog false)}]]]
                  ;; confirm dialog
                  [:guard [:predicate {:func (bt/bb-getter-in [:confirm-dialog])}]
                   [:parallel {:policy :select :rerun-children true} ;; we have to rerun to work around the html5 spec bug
                    [:on-event {:event :cancel-pressed :wait? true}
                     [:update {:func (bt/bb-updater assoc
                                                    :restore-button true
                                                    :confirm-dialog false)}]]
                    [:on-event {:event :confirm-pressed :wait? true}
                     [:send-event {:event :open-file-dialog}]]]]
                  ;; got file data
                  [:on-event {:event :got-file-data :bind-arg :file-data
                              :wait? true}
                   [:sequence
                    [:update {:func (bt/bb-updater assoc
                                                   :confirm-dialog false
                                                   :restoring-dialog true)}]
                    [:send-event {:event :restore-file :arg (bt/var-getter :file-data)}]]]
                  ;; got restore operation result
                  [:on-event {:event :restore-result :bind-arg :result :wait? true}
                   [:update {:func #(bt/bb-update % assoc
                                                  :restoring-dialog false
                                                  :restore-button true
                                                  :success-dialog (= :success (bt/get-var % :result))
                                                  :error-dialog (= :error (bt/get-var % :result)))}]]
                  ;; success dialog
                  [:guard [:predicate {:func (bt/bb-getter-in [:success-dialog])}]
                   [:on-event {:event :ok-pressed :wait? true}
                    [:update {:func (bt/bb-updater assoc :success-dialog false)}]]]
                  ;; error dialog
                  [:guard [:predicate {:func (bt/bb-getter-in [:error-dialog])}]
                   [:on-event {:event :ok-pressed :wait? true}
                    [:update {:func (bt/bb-updater assoc :error-dialog false)}]]]]]
                bt/hiccup->context bt/tick)
        send-event (fn send-event
                     ([ctx event]
                      (send-event ctx event nil))
                     ([ctx event arg]
                      (-> (bt/send-event ctx event arg) bt/tick)))
        send-events (fn send-events [ctx & events]
                      (reduce #(apply send-event %1 %2)
                              ctx (map #(if (seqable? %) % [%]) events)))]
    (is ctx)
    
    (is (-> ctx (bt/bb-get-in [:restore-button])))
    
    (is (= :running (-> (send-events ctx :restore-pressed)
                        bt/get-status)))
    
    (is (not (-> (send-events ctx :restore-pressed)
                 (bt/bb-get-in [:restore-button]))))
    
    (is (-> (send-events ctx :restore-pressed)
            (bt/bb-get-in [:confirm-dialog])))
    
    (is (not (-> (send-events ctx :restore-pressed :cancel-pressed)
                 (bt/bb-get-in [:confirm-dialog]))))
    
    (is (-> (send-events ctx :restore-pressed :cancel-pressed)
            (bt/bb-get-in [:restore-button])))
    
    (is (not (-> (send-events ctx :restore-pressed :cancel-pressed :restore-pressed)
                 (bt/bb-get-in [:restore-button]))))
    
    (is (-> (send-events ctx :restore-pressed :cancel-pressed :restore-pressed)
            (bt/bb-get-in [:confirm-dialog])))
    
    (is (-> (send-events ctx :restore-pressed :confirm-pressed)
            (bt/bb-get-in [:confirm-dialog])))
    
    (is (= [[:open-file-dialog nil]]
           (-> (send-events ctx :restore-pressed :confirm-pressed)
               bt/get-events)))
    
    (is (= [[:open-file-dialog nil]]
           (-> (send-events ctx :restore-pressed :confirm-pressed :confirm-pressed)
               bt/get-events)))
    
    (is (not (-> (send-events ctx :restore-pressed :confirm-pressed :cancel-pressed)
                 (bt/bb-get-in [:confirm-dialog]))))
    
    (is (-> (send-events ctx :restore-pressed :confirm-pressed :cancel-pressed)
            (bt/bb-get-in [:restore-button])))

    (let [ctx (send-events ctx :restore-pressed :confirm-pressed [:got-file-data :some-data])]
      ;; this new context has received file data
      
      (is (= [[:restore-file :some-data]]
             (-> ctx bt/get-events)))
      
      (is (not (-> ctx (bt/bb-get-in [:confirm-dialog]))))
      
      (is (-> ctx (bt/bb-get-in [:restoring-dialog])))
      
      (is (not (-> (send-events ctx [:restore-result :success])
                   (bt/bb-get-in [:restoring-dialog]))))
      
      (is (-> (send-events ctx [:restore-result :success])
              (bt/bb-get-in [:success-dialog])))
      
      (is (not (-> (send-events ctx [:restore-result :success])
                   (bt/bb-get-in [:error-dialog]))))
      
      (is (-> (send-events ctx [:restore-result :success])
              (bt/bb-get-in [:restore-button])))

      (is (not (-> (send-events ctx [:restore-result :error])
                   (bt/bb-get-in [:restoring-dialog]))))
      
      (is (-> (send-events ctx [:restore-result :error])
              (bt/bb-get-in [:error-dialog])))
      
      (is (not (-> (send-events ctx [:restore-result :error])
                   (bt/bb-get-in [:success-dialog]))))
      
      (is (-> (send-events ctx [:restore-result :success])
              (bt/bb-get-in [:restore-button])))

      (is (not (-> (send-events ctx [:restore-result :success] :ok-pressed)
                   (bt/bb-get-in [:success-dialog]))))
      
      (is (not (-> (send-events ctx [:restore-result :error] :ok-pressed)
                   (bt/bb-get-in [:error-dialog]))))

      (is (not (-> (send-events ctx [:restore-result :error] :restore-pressed)
                   (bt/bb-get-in [:restore-button]))))
      
      (is (-> (send-events ctx [:restore-result :error] :restore-pressed)
              (bt/bb-get-in [:confirm-dialog])))

      (is (not (-> (send-events ctx [:restore-result :error] :restore-pressed)
                   (bt/bb-get-in [:error-dialog])))))))
