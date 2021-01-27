(ns crux.select-mango-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]
            crux.select))

(defn select [q & [{:as opts}]]
  (crux.select/select (api/db *api*) (merge opts {:selector q})))

(defn- with-test-data [f]
  (let [{:keys [docs]} (json/parse-string (slurp (io/resource "data/select_test.json")) keyword)]
    (fix/transact! *api* docs)
    (f)))

(t/use-fixtures :each fix/with-node with-test-data)

;; Ported from Couch:

(t/deftest test-simple-find
  (let [docs (select {:age {:$lt 35}})]

    (t/is (= 3 (count docs)))

    ;; TODO, they do ordering
    ;; assert docs[0]["user_id"] == 9
    ;; assert docs[1]["user_id"] == 1
    ;; assert docs[2]["user_id"] == 7

    (t/is (= #{9 1 7} (set (map :user_id docs))))))

(t/deftest test-multi-cond-find
  (let [docs (select {:manager true, :user_id 7})]
    (t/is (= 1 (count docs)))
    (t/is (= 7 (:user_id (first docs))))))

(t/deftest test-multi-cond-or
  (let [docs (select {:$and [{:age {:$gte 75}}]
                      :$or [{:firstName "Mathis"} {:firstName "Whitley"}]})]
    (t/is (= 2 (count docs)))
    (t/is (= #{11 13} (set (map :user_id docs))))))

(t/deftest test-limit
  (let [docs (select {:age {:$gt 0}})]
    (assert (= 15 (count docs)))
    (doseq [n [0 1 5 14]]
      (t/is (= n (count (select {:age {:$gt 0}} {:limit n})))))))

(t/deftest test-offset
  (let [docs (select {:age {:$gt 0}})]
    (assert (= 15 (count docs)))
    (doseq [n [0 1 5 14]]
      (t/is (= (- 15 n) (count (select {:age {:$gt 0}} {:offset n})))))))

(t/deftest test-sort
  (let [docs (select {:age {:$gt 0}})]
    (t/is (= (sort-by :age docs) (select {:age {:$gt 0}} {:order-by [{:age :asc}]})))
    (t/is (= (reverse (sort-by :age docs)) (select {:age {:$gt 0}} {:order-by [{:age :desc}]})))))
