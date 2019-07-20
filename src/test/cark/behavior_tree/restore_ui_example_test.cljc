(ns cark.behavior-tree.restore-ui-example-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]
            [cark.behavior-tree.state-machine :as sm]
            [cark.behavior-tree.context :as ctx]
            [clojure.set :as set]))

;; we define a backup restore page
;;
;; - a button "restore" will start the process
;; - first a confirmation dialog will open
;; - restore button is grayed out
;; - the dialog cancel button makes the confirm dialog disapear
;; - the dialog's confirm button will start the open file dialog
;; - html5 only gives a result when the user selects a file, not when he
;; cancels the open file dialog,we need to work around this
;; - when we have the file data, we close the confirmation dialog and send a restore event
;; - we open a "restoring" dialog
;; - we receive a result that might be
;; - success : open a success dialog having an ok button, activate restore button
;; - failure : open a failure dialog having an ok button, activate restore button
;;

(def ctx
  (-> (sm/make [:sm] :start
        (sm/state :start
          (sm/enter-event [:update {:func (bt/bb-updater-in [:flags] set/union #{:restore-button})}])
          (sm/event :restore-pressed (sm/transition :restore))
          (sm/event :ok-pressed
            [:update {:func (bt/bb-updater-in [:flags] set/difference #{:success-dialog
                                                                        :error-dialog})}]))
        (sm/state :restore
          (sm/enter-event [:update {:func (bt/bb-assocer-in [:flags] #{:confirm-dialog})}])
          (sm/event :cancel-pressed
            [:sequence
             [:update {:func (bt/bb-assocer-in [:flags] #{})}]
             (sm/transition :start)])
          (sm/event :got-file-data
            [:sequence
             [:send-event {:event :restore-file :arg sm/event-arg}]
             [:update {:func (bt/bb-assocer-in [:flags] #{:restoring-dialog})}]])
          (sm/event :restore-result
            [:sequence
             [:update {:func
                       #(cond-> (bt/bb-update % assoc :flags #{:restore-button})
                          (= :success (first (sm/event-arg %))) (bt/bb-update-in [:flags] conj :success-dialog)
                          (= :error (first (sm/event-arg %))) (-> (bt/bb-update-in [:flags] conj :error-dialog)
                                                                  (bt/bb-update assoc :error (second (sm/event-arg %)))))}]
             (sm/transition :start)])))
      bt/hiccup->context bt/tick))

(deftest restore-test
  (let [ctx (-> ctx)
        send-event (fn send-event
                     ([ctx event]
                      (send-event ctx event nil))
                     ([ctx event arg]
                      (bt/send-event ctx event arg)))
        send-events (fn send-events [ctx & events]
                      (reduce #(apply send-event %1 %2)
                              ctx (map #(if (seqable? %) % [%]) events)))]
    (is ctx)
    
    (is (-> ctx (bt/bb-get-in [:flags :restore-button])))
    
    (is (= :running (-> (send-events ctx :restore-pressed)
                        bt/get-status)))
    
    (is (not (-> (send-events ctx :restore-pressed)
                 (bt/bb-get-in [:flags :restore-button])))) 
    
    (is (-> (send-events ctx :restore-pressed)
            (bt/bb-get-in [:flags :confirm-dialog])))
    
    (is (not (-> (send-events ctx :restore-pressed :cancel-pressed)
                 (bt/bb-get-in [:flags :confirm-dialog]))))
    
    (is (-> (send-events ctx :restore-pressed :cancel-pressed)
            (bt/bb-get-in [:flags :restore-button])))
    
    (is (not (-> (send-events ctx :restore-pressed :cancel-pressed :restore-pressed)
                 (bt/bb-get-in [:flags :restore-button]))))
    
    (is (-> (send-events ctx :restore-pressed :cancel-pressed :restore-pressed)
            (bt/bb-get-in [:flags :confirm-dialog])))

    (let [ctx (send-events ctx :restore-pressed [:got-file-data :some-data])]
      ;; this new context has received file data
      
      (is (= [[:restore-file :some-data]]
             (-> ctx bt/get-events)))
      
      (is (not (-> ctx (bt/bb-get-in [:flags :confirm-dialog]))))
      
      (is (-> ctx (bt/bb-get-in [:flags :restoring-dialog])))
      
      (is (not (-> (send-events ctx [:restore-result [:success]])
                   (bt/bb-get-in [:flags :restoring-dialog]))))
      
      (is (-> (send-events ctx [:restore-result [:success]])
              (bt/bb-get-in [:flags :success-dialog])))
      
      (is (not (-> (send-events ctx [:restore-result [:success]])
                   (bt/bb-get-in [:flags :error-dialog]))))
      
      (is (-> (send-events ctx [:restore-result [:success]])
              (bt/bb-get-in [:flags :restore-button])))

      (is (not (-> (send-events ctx [:restore-result [:error :bleh]])
                   (bt/bb-get-in [:flags :restoring-dialog]))))
      
      (is (-> (send-events ctx [:restore-result [:error :blah]])
              (bt/bb-get-in [:flags :error-dialog])))
      
      (is (not (-> (send-events ctx [:restore-result [:error :blah]])
                   (bt/bb-get-in [:flags :success-dialog]))))
      
      (is (-> (send-events ctx [:restore-result [:success]])
              (bt/bb-get-in [:flags :restore-button])))

      (is (not (-> (send-events ctx [:restore-result [:success]] :ok-pressed)
                   (bt/bb-get-in [:flags :success-dialog]))))
      
      (is (not (-> (send-events ctx [:restore-result [:error :blah]] :ok-pressed)
                   (bt/bb-get-in [:flags :error-dialog]))))

      (is (not (-> (send-events ctx [:restore-result [:error :blah]] :restore-pressed)
                   (bt/bb-get-in [:flags :restore-button]))))
      
      (is (-> (send-events ctx [:restore-result [:error :blah]] :restore-pressed)
              (bt/bb-get-in [:flags :confirm-dialog])))

      (is (not (-> (send-events ctx [:restore-result [:error :blah]] :restore-pressed)
                   (bt/bb-get-in [:flags :error-dialog])))))))
