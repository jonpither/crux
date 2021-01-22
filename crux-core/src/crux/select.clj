(ns crux.select
  (:require [clojure.walk :refer [postwalk]]
            [crux.api :as api]))

(def operators {:$eq '=
                :$gt '>
                :$gte '>=
                :$lt '<
                :$lte '<=})

(def conditions #{:$and :$not :$or})

(def field? (complement operators))

(defn ->ast [selector]
  (into []
        (for [[k v] selector]
          (if (conditions k)
            [:condition k (vec (mapcat ->ast (if (vector? v) v [v])))]
            (if (field? k)
              (let [[op operand] (first (if (map? v) v {:$eq v}))]
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

(defn- ->where [field->vars node]
  (if (vector? (first node))
    ;; Implicit and:
    (map (partial ->where field->vars) node)

    (case (first node)
      :field
      (let [[_ field op literal] node]
        [(list (operators op) (field->vars field) literal)])

      :condition
      (let [[_ condition args] node]
        (case condition
          :$and
          (apply list 'and (map (partial ->where field->vars) args))
          :$not
          (apply list 'not (map (partial ->where field->vars) args))
          :$or
          (apply list 'or (map (partial ->where field->vars) args)))))))

(defn ->datalog [ast]
  (let [field->vars (into {} (map vector (collect-fields ast) (repeatedly gensym)))]
    {:find ['e]
     :where (into (vec (for [[field var] field->vars]
                         ['e field var]))
                  ;; Unpack top level 'and
                  (reduce into []
                          (for [clause (->where field->vars ast)]
                            (if (= 'and (first clause)) (rest clause) [clause]))))}))

(defn select [db q]
  (map (partial api/entity db) (map first (api/q db (doto (->datalog (->ast q)) prn)))))

(comment
  (collect-fields (->ast {:age {:$gt 9}}))
  (->ast {:$not {:age {:$gt 9}}})
  (->ast {:name "Ivan"})

  (->datalog (->ast {:name "Ivan"}))
  (->datalog (->ast {:$not {:name "Ivan"}}))
  (->datalog (->ast {:age {:$gt 9}}))
  (->datalog (->ast {:manager true, :user_id 7}))

  (->ast {:$and [{:age {:$gte 75}}]
          :$or [{:firstName "Mathis"} {:firstName "Whitley"}]})

  [[:condition :$and [[[:field :age :$gte 75]]]]
   [:condition :$or [[[:field :firstName :$eq "Mathis"]]
                     [[:field :firstName :$eq "Whitley"]]]]]

  (->datalog (->ast {:$and [{:age {:$gte 75}}]
                     :$or [{:firstName "Mathis"} {:firstName "Whitley"}]}))

  (->datalog (->ast {:$and [{:name "Ivan" :surname "Bob"}]}))

  {:find [e], :where [[e :name G__95625]
                      [e :surname G__95626]
                      [[(= G__95625 "Ivan")] [(= G__95626 "Bob")]]]})
