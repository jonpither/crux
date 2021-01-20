(ns crux.select-test
  (:require [clojure.test :as t]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]))

(t/use-fixtures :each fix/with-node)

(defn- select->datalog [q]
  {:find ['e]
   :where (reduce into [] (for [[k v] q]
                            (if (map? v)
                              (let [[op v] (first v)
                                    a (gensym k)]
                                [['e k a]
                                 [(list ({:$gt '>} op) a v)]])
                              [['e k v]])))})

(defn- select [q]
  (let [db (api/db *api*)]
    (map (partial api/entity db) (map first (api/q db (doto (select->datalog q) prn))))))

(t/deftest test-select
  ;; https://docs.couchdb.org/en/stable/api/database/find.html
  (let [ivan {:crux.db/id :ivan :name "Ivan" :surname "Ivanof"}]
    (fix/transact! *api* (fix/people [ivan]))

    (t/testing "Single field"
      (t/is (= :ivan (:crux.db/id (first (select {:name "Ivan"})))))
      (t/is (not (seq (select {:name "Ivana"})))))

    (t/testing "multi field"
      (t/is (= :ivan (:crux.db/id (first (select {:name "Ivan"
                                                  :surname "Ivanof"}))))))))

(t/deftest test-operator
  ;; https://docs.couchdb.org/en/stable/api/database/find.html#implicit-operators
  (let [ivan {:crux.db/id :ivan :age 10}]
    (fix/transact! *api* (fix/people [ivan]))

    (t/testing "Single field"
      (t/is (= :ivan (:crux.db/id (first (select {:age {:$gt 9}})))))
      (t/is (not (seq (select {:age {:$gt 11}})))))))


;; todo, does Couch have a test suite we can use?
