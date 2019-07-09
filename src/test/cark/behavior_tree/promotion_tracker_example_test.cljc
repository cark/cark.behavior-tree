(ns cark.behavior-tree.promotion-tracker-example-test
  (:require [clojure.test :as t :refer [deftest is]]
            [cark.behavior-tree.core :as bt]))


;; We define a promotion tracker for a web site
;;
;; We want to offer a promotion to the indecisive visitors.
;; visitors are considered indecisive when they first visit the
;; shopping cart, then return to the shop, then back to the shopping cart.
;;
;; The visitors might navigate to other pages in the meantime, but we do not care for those.
;;
;; We later simulate an API, saving the working set to an atom


(def tracker
  (-> [:map {:seq [:shopping-cart :shop :shopping-cart]
             :bind-item :page-name}
       [:until-success
        [:on-event {:event :navigate :wait? true :bind-arg :page}
         [:predicate {:func #(= (bt/get-var % :page) (bt/get-var % :page-name))}]]]]
      bt/hiccup->context))

(deftest promotion-tracker-test
  (is (= :running (-> tracker
                      (bt/send-event :navigate :shopping-cart) bt/tick
                      (bt/send-event :navigate :shop) bt/tick
                      bt/get-status)))
  (is (= :success (-> tracker
                      (bt/send-event :navigate :shopping-cart) bt/tick
                      (bt/send-event :navigate :shop) bt/tick
                      (bt/send-event :navigate :shopping-cart) bt/tick
                      bt/get-status)))
  (is (= :success (-> tracker
                      (bt/send-event :navigate :shopping-cart) 
                      (bt/send-event :navigate :shop) 
                      (bt/send-event :navigate :shopping-cart)
                      bt/tick
                      bt/get-status)))
  (is (= :success (-> tracker
                      (bt/send-event :navigate :somewhere-else)
                      (bt/send-event :navigate :somewhere-else)
                      (bt/send-event :navigate :shopping-cart)
                      (bt/send-event :navigate :somewhere-else)
                      (bt/send-event :navigate :shop)
                      (bt/send-event :navigate :somewhere-else)
                      (bt/send-event :navigate :somewhere-else)
                      (bt/send-event :navigate :shopping-cart)
                      bt/tick
                      bt/get-status))))

;; We define a quick and dirty api, saving the behavior tree's working set to an atom

(def conn (atom (bt/extract-db tracker)))

(defn reset
  "Resets the content of the database"
  []
  (reset! conn (bt/extract-db tracker)))

(defn with-db
  "Executes func with the context as its first argument and args as next arguments, 
updating the connection witht the result working set."
  [func & args]
  (swap! conn #(-> (apply func (bt/merge-db tracker %) args)
                   bt/extract-db)))

(defn from-db
  "Extracts some data from the database."
  [func & args]
  (apply func (bt/merge-db tracker @conn) args))

(defn navigate
  "Navigates to the provided page"
  [ctx page]
  (-> ctx (bt/send-event :navigate page) bt/tick))

(defn db-navigate
  "Navigates to the specified page, returning a boolean which is true when we should offer a promotion"
  [page]
  (with-db navigate page)
  (= (from-db bt/get-status) :success))

(deftest api-test
  (reset)
  (is (not (db-navigate :shopping-cart)))
  (is (not (db-navigate :about-us)))
  (is (not (db-navigate :shop)))
  (is (not (db-navigate :contact)))
  (is (db-navigate :shopping-cart)))

