(ns crux.select-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :as t]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]))

(t/use-fixtures :each fix/with-node)

(def operands {:$eq '=
               :$gt '>
               :$gte '>=
               :$lt '<
               :$lte '<=})

;; ;; so nice to get the spec working
;; (s/def ::nested #{:$not})
;; (s/def ::operator (set (keys operands)))
;; (s/def ::val (s/map-of ::operator any?))
;; (s/def ::query (s/map-of keyword ::val))

;; interesting. Or is going to be quite hard
;; {
;;     "year": 1977,
;;     "$or": [
;;         { "director": "George Lucas" },
;;         { "director": "Steven Spielberg" }
;;     ]
;; }

;; Not
;; {
;;     "year": {
;;         "$gte": 1900
;;     },
;;     "year": {
;;         "$lte": 1903
;;     },
;;     "$not": {
;;         "year": 1901
;;     }
;; }


;;
(defn- ->where [selector]
  (reduce into []
          (for [[k v] selector]
            (do
              (println "h" (= :$not k) k v)
              (case k
                :$not
                [(apply list 'not
                        (mapcat ->where v))]
                (let [[op v] (if (coll? v) (first v) [:$eq v])
                      a (gensym (name k))]
                  [['e k a]
                   [(list (operands op) a v)]]))))))

(defn- select->datalog [q]
  {:find ['e]
   :where (->where q)})

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
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :age 10}]))

  (t/testing "eq"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$eq 10}}))))))

  (t/testing "gt"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$gt 9}})))))
    (t/is (not (seq (select {:age {:$gt 11}})))))

  (t/testing "lt"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$lt 11}})))))
    (t/is (not (seq (select {:age {:$lt 9}})))))

  (t/testing "than equal"
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$lte 11}})))))
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$lte 10}})))))
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$gte 9}})))))
    (t/is (= :ivan (:crux.db/id (first (select {:age {:$gte 10}}))))))

  #_(t/is (thrown-with-msg? clojure.lang.ExceptionInfo #"Spec assertion failed"
                          (select {:age {:$unknown 11}}))))

(t/deftest test-not
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :name "Ivan"}
                                    {:crux.db/id :fred :name "Fred"}
                                    {:crux.db/id :jim :name "Jim"}]))

  (println "got" (select {:$not [{:name "Ivan"}]}))

  (t/is (= #{:fred :jim} (set (map :crux.db/id (select {:$not [{:name "Ivan"}]}))))))

#_(t/deftest test-or
  ;; "$or": [
  ;;         { "director": "George Lucas" },
  ;;         { "director": "Steven Spielberg" }
  ;;         ]
  (fix/transact! *api* (fix/people [{:crux.db/id :ivan :name "Ivan"}
                                    {:crux.db/id :fred :name "Fred"}
                                    {:crux.db/id :jim :name "Jim"}]))

  (t/is (= #{:ivan :fred} (set (map :crux.db/id (select {:$or [{:name "Ivan"} {:name "Fred"}]}))))))

;; todo in
;; todo elemMatch
;; todo fields?
;; todo, does Couch have a test suite we can use?
