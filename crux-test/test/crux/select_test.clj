(ns crux.select-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as t]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]))

(t/use-fixtures :each fix/with-node)

(s/def ::operator #{:$eq :$lt :$gt})
(s/def ::val (s/map-of ::operator any?))
(s/def ::query (s/map-of keyword ::val))

(defn- select->datalog [q]
  (let [q (into {} (map (fn [[k v]]
                          [k (if (map? v) v {:$eq v})])) q)]
    (s/assert ::query q)
    {:find ['e]
     :where (reduce into [] (for [[k v] q]
                              (if (map? v)
                                (let [[op v] (first v)
                                      a (gensym k)]
                                  [['e k a]
                                   [(list ({:$eq '=
                                            :$gt '>
                                            :$lt '<} op) a v)]])
                                [['e k v]])))}))

(defn- select [q]
  (let [db (api/db *api*)]
    (map (partial api/entity db) (map first (api/q db (doto
                                                          (select->datalog q) prn))))))

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
  (let [ivan {:crux.db/id :ivan :age 10}]
    (fix/transact! *api* (fix/people [ivan]))

    (t/testing "eq"
      (t/is (= :ivan (:crux.db/id (first (select {:age {:$eq 10}}))))))

    (t/testing "gt"
      (t/is (= :ivan (:crux.db/id (first (select {:age {:$gt 9}})))))
      (t/is (not (seq (select {:age {:$gt 11}})))))

    (t/testing "lt"
      (t/is (= :ivan (:crux.db/id (first (select {:age {:$lt 11}})))))
      (t/is (not (seq (select {:age {:$lt 9}})))))

    (t/is (thrown-with-msg? clojure.lang.ExceptionInfo #"Spec assertion failed"
                            (select {:age {:$unknown 11}})))))

;; todo in
;; todo elemMatch
;; todo fields?
;; todo, does Couch have a test suite we can use?
