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
                (when (vector? x)
                  (let [[t f o v] x]
                    (when (and (= :field t)
                               (not (and (= :$exists o) (false? v))))
                      (swap! fields conj f))))
                x)
              ast)
    @fields))

(defn- root? [ast]
  (and (vector? ast) (and (= :root (first ast)))))

(defn- and? [ast]
  (and (vector? ast) (and (= :condition (first ast)) (= :$and (second ast)))))

(defn- unpack-nested-ands [ast]
  (clojure.walk/postwalk
   (fn [x]
     (if (or (root? x) (and? x))
       (conj (vec (drop-last x))
             (reduce into []
                     (for [x (last x)]
                       (if (and? x)
                         (last x)
                         [x]))))
       x))
   ast))

;; Taken from calcite.clj
(defn- ground-vars [or-statement]
  (let [vars (distinct (mapcat #(filter symbol? (rest (first %))) or-statement))]
    (vec
     (for [clause or-statement]
       (apply list 'and clause  (map #(vector (list 'identity %)) vars))))))

(defn- ->where [field->vars node]
  (case (first node)
    :root
    (map (partial ->where field->vars) (last node))

    :field
    (let [[_ field op literal] node]
      (if (= :$exists op)
        (when (not literal)
          (list 'not ['e field]))
        [(list (operators op) (field->vars field) literal)]))

    :condition
    (let [[_ condition args] node]
      (when-let [sub-clauses (map (partial ->where field->vars) args)]
        (case condition
          :$and
          (apply list 'and sub-clauses)
          :$not
          (apply list 'not sub-clauses)
          :$or
          (apply list 'or (ground-vars sub-clauses)))))))

(defn ->datalog [{:keys [selector limit offset order-by]}]
  (let [ast (unpack-nested-ands [:root (->ast selector)])
        field->vars (into {} (map vector (collect-fields ast) (repeatedly gensym)))]
    (merge
     {:find (into ['e] (mapv (fn [[k]]
                               (field->vars k))
                             (apply merge order-by)))
      :where (into []
                   (concat [['e :crux.db/id]]
                           (for [[field var] field->vars]
                             ['e field var])
                           (remove nil? (->where field->vars ast))))}
     (when limit {:limit limit})
     (when offset {:offset offset})
     (when order-by {:order-by (mapv (fn [[k direction]]
                                       [(field->vars k) direction])
                                     (apply merge order-by))}))))

(defn select [db {:keys [fields] :as q}]
  (->> (doto (->datalog q) prn)
       (api/q db)
       (map first)
       (map (partial api/entity db))
       (map #(if fields (select-keys % fields) %))))

(comment
  (collect-fields (->ast {:age {:$gt 9}}))
  (->ast {:$not {:age {:$gt 9}}})
  (->ast {:name "Ivan"})

  (->datalog {:name "Ivan"})
  (->datalog {:$not {:name "Ivan"}})
  (->datalog {:age {:$gt 9}})
  (->datalog {:manager true, :user_id 7})

  (->ast {:$and [{:age {:$gte 75}}]
          :$or [{:firstName "Mathis"} {:firstName "Whitley"}]})

  [[:condition :$and [[[:field :age :$gte 75]]]]
   [:condition :$or [[[:field :firstName :$eq "Mathis"]]
                     [[:field :firstName :$eq "Whitley"]]]]]

  (->datalog {:$and [{:age {:$gte 75}}]
              :$or [{:firstName "Mathis"} {:firstName "Whitley"}]})

  (->datalog {:$and [{:name "Ivan" :surname "Bob"}]})

  {:find [e], :where [[e :name G__95625]
                      [e :surname G__95626]
                      [[(= G__95625 "Ivan")] [(= G__95626 "Bob")]]]})
