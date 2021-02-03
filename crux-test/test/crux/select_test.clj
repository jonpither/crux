(ns crux.select-test
  (:require [clojure.test :as t]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]
            [crux.fixtures.select :refer [select ex-data-thrown?]]
            crux.select))

(t/use-fixtures :each fix/with-node)

(t/deftest test-select
  ;; https://docs.couchdb.org/en/stable/api/database/find.html
  (let [ivan {:crux.db/id :ivan :name "Ivan" :surname "Ivanof"}]
    (fix/transact! *api* (fix/people [ivan]))

    (t/testing "Single field"
      (t/is (= :ivan (:crux.db/id (first (select {:name "Ivan"})))))
      (t/is (not (seq (select {:name "Ivana"})))))

    (t/testing "multi field"
      (t/is (= :ivan (:crux.db/id (first (select {:name "Ivan" :surname "Ivanof"}))))))))

(t/deftest test-operator
  ;; https://docs.couchdb.org/en/stable/api/database/find.html#implicit-operators
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :age 10}]))

  (t/testing "eq"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$eq 10}}))))))

  (t/testing "gt"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$gt 9}})))))
    (t/is (not (seq (select {:age {:$gt 11}})))))

  (t/is (not (seq (select {:age {:$lt 9}}))))

  (t/testing "than equal"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$lte 11}})))))
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$lte 10}})))))
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$gte 9}})))))
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$gte 10}})))))))

(t/deftest test-and
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :name "Ivan"}
                                    {:crux.db/id :fred :name "Fred"}
                                    {:crux.db/id :jim :name "Jim" :surname "Bob"}]))

  (t/is (= #{:jim} (set (map :crux.db/id (select {:$and [{:name "Jim"} {:surname "Bob"}]}))))))

(t/deftest test-not
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :name "Ivan"}
                                    {:crux.db/id :fred :name "Fred"}
                                    {:crux.db/id :jim :name "Jim"}]))

  (t/is (= #{:fred :jim} (set (map :crux.db/id (select {:$not [{:name "Ivan"}]}))))))

(t/deftest test-or
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :name "Ivan"}
                                    {:crux.db/id :fred :name "Fred"}
                                    {:crux.db/id :jim :name "Jim"}]))

  (t/is (= #{:ivan :fred} (set (map :crux.db/id (select {:$or [{:name "Ivan"} {:name "Fred"}]}))))))

(t/deftest test-nested
  (fix/transact! *api* (fix/people [{:crux.db/id :jons :firstName "Jon" :lastName "Duggen" :age 10}
                                    {:crux.db/id :jons2 :firstName "Jon" :lastName "Flow" :age 11}
                                    {:crux.db/id :jonb :firstName "Jon" :lastName "Smith" :age 10}]))
  (t/is (= #{:jons} (set (map :crux.db/id (select {:$and [{:age {:$lte 10}}
                                                          {:$and [{:firstName "Jon"}
                                                                  {:$not {:lastName "Smith"}}]}]}))))))

(t/deftest test-bad-operator
  (ex-data-thrown?
   {:error :invalid-operator, :operator :$notAnd}
   (select {:name {:$notAnd 10}})))

(t/deftest test-bad-selector
  (ex-data-thrown?
   {:operator :$lte, :error :invalid-selector}
   (select {:$lte 10})))

(t/deftest test-lookup
  (fix/transact! *api* [{:crux.db/id :mars :type "planet" :name "Mars"}
                        {:crux.db/id :jon :firstName "Jon" :planet :mars}
                        {:crux.db/id :joe :firstName "Joe" :planet :earth}
                        {:crux.db/id :jim :firstName "Jim" :planet :mars}])

  (t/is (= [{:crux.db/id :mars :type "planet" :name "Mars"
             :people [{:crux.db/id :jim :firstName "Jim" :planet :mars}
                      {:crux.db/id :jon :firstName "Jon" :planet :mars}]}]
           (select {:type "planet"}
                   {:lookup {:let {:crux.db/id :$planet}
                             :from {:planet :$$planet}
                             :as :people}})))

  (t/is (not (seq (select {:type "planet"
                           :crux.db/id {:$exists false}}
                          {:lookup {:let {:crux.db/id :$planet}
                                    :from {:planet :$$planet}
                                    :as :people}})))))

;; todo NS keywords?
;; todo re-add spec to AST to check for operators etc, or throw human meaninful msgs
;; todo in
;; todo elemMatch
;; todo fields?
;; mysecretpassword
;; use ns crux.error

;; spec options:
;; 1 not spec, we put asserts in the code
;; 2 spec the AST
;; 3 small transformation on the selector, to make speccable
;;  $not : blag -> [$not :blag] jdt - slack
;;  {$not foo :age 10} -> [{$not foo} {:age 10}]

;; todo - barf if an operator in the field position

;; a good demo is http. Just hack it tf in, maybe ask Dan or jmo how to fire up from the command line, or jdt. cba now, tired.

;; db.orders.aggregate([
;;    {
;;       $lookup:
;;          {
;;            from: "warehouses",
;;            let: { order_item: "$item", order_qty: "$ordered" },
;;            pipeline: [
;;               { $match:
;;                  { $expr:
;;                     { $and:
;;                        [
;;                          { $eq: [ "$stock_item",  "$$order_item" ] },
;;                          { $gte: [ "$instock", "$$order_qty" ] }
;;                        ]
;;                     }
;;                  }
;;               },
;;               { $project: { stock_item: 0, _id: 0 } }
;;            ],
;;            as: "stockdata"
;;          }
;;     }
;; ])
