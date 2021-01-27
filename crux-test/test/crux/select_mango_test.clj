(ns crux.select-mango-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]
            [crux.fixtures.select :refer [select]]
            crux.select))

(defn- with-test-data [f]
  (let [{:keys [docs]} (json/parse-string (slurp (io/resource "data/select_test.json")) keyword)]
    (fix/transact! *api* docs)
    (f)))

(t/use-fixtures :each fix/with-node with-test-data)

;; Ported from Couch:

(t/deftest test-simple-find
  (let [docs (select {:age {:$lt 35}})]
    (t/is (= 3 (count docs)))
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

(t/deftest test-sort-desc-complex
  (t/is (= ["Lyria" "Globoil" "Affluex"]
           (map :company (select {:company {:$lt "M"}
                                  :$or [{:company "Dreamia"} {:manager true}]}
                                 {:order-by [{:company :desc} {:manager :desc}]}))))

  (t/is (= ["Affluex" "Globoil" "Lyria"]
           (map :company (select {:company {:$lt "M"}
                                  :$or [{:company "Dreamia"} {:manager true}]}
                                 {:order-by [{:company :asc} {:manager :desc}]})))))

(t/deftest test-sort-exists-true
  (let [docs (select {:age {:$gt 0 :$exists true}} {:order-by [{:age :asc}]})]
    (t/is (= (sort (map :age docs))
             (map :age docs)))))

(t/deftest test-fields
  (let [docs (select {:age {:$gt 0}} {:fields [:user_id]})]
    (assert (= 15 (count docs)))
    (t/is (= #{:user_id} (set (mapcat keys docs))))))

(t/deftest test-empty
  (t/is (= 15 (count (select {})))))


;; 06-basic-text-tests -------------------------

(t/deftest test-exists-field
  (let [docs (select {:exists_field {:$exists true}})]
    (t/is (= 2 (count docs)))
    (t/is (= #{7 8} (set (map :user_id docs)))))

  (let [docs (select {:exists_field {:$exists false}})]
    (t/is (= 13 (count docs)))
    (t/is (empty? (filter #{7 8} (map :user_id docs))))))
