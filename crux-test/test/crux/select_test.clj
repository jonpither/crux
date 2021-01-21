(ns crux.select-test
  (:require [clojure.test :as t]
            [clojure.walk :refer [postwalk]]
            [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]))

(t/use-fixtures :each fix/with-node)

(def operators {:$eq '=
                :$gt '>
                :$gte '>=
                :$lt '<
                :$lte '<=})

(def conditions #{:$and :$not})

(def field? (complement operators))

(defn ->ast [selector]
  (reduce into [:condition :$and]
          (for [[k v] selector]
            (if (conditions k)
              [[:condition k (vec (mapcat ->ast (if (vector? v) v [v])))]]
              (if (field? k)
                (for [[op operand] (if (map? v) v {:$eq v})]
                  [:field k op operand])
                k)))))

(defn- collect-fields [ast]
  (let [fields (atom #{})]
    (postwalk (fn [x]
                (when (and (vector? x) (= :field (first x)))
                  (swap! fields conj (second x)))
                x)
              ast)
    @fields))

(comment
  (collect-fields (->ast {:age {:$gt 9}}))
  (->ast {:$not {:age {:$gt 9}}}))

(defn- ->where [field->vars node]
  (case (first node)
    :field
    (let [[_ field op literal] node]
      [(list (operators op) (field->vars field) literal)])

    :condition
    (case (second node)
      :$and
      (mapv (partial ->where field->vars) (drop 2 node))
      :$not
      (apply list 'not (mapcat (partial ->where field->vars) (drop 2 node))))))

(defn ->datalog [selector]
  (let [ast (->ast selector)
        field->vars (into {} (map vector (collect-fields ast) (repeatedly gensym)))]
    {:find ['e]
     :where (into (vec (for [[field var] field->vars]
                         ['e field var]))
                  (->where field->vars ast))}))

(comment
  (->datalog {:name "Ivan"})
  (->datalog {:age {:$gt 9}}))

(defn- select [q]
  (let [db (api/db *api*)]
    (map (partial api/entity db) (map first (api/q db (doto (->datalog q) prn))))))

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

;; todo re-add spec to AST to check for operators etc, or throw human meaninful msgs
;; todo in
;; todo elemMatch
;; todo fields?
;; todo, does Couch have a test suite we can use?
