(ns crux.select
  (:require [clojure.walk :refer [postwalk]]
            [crux.api :as api]))

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

(defn select [db q]
  (map (partial api/entity db) (map first (api/q db (doto (->datalog q) prn)))))
