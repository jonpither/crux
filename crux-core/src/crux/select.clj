(ns crux.select
  (:require [clojure.walk :refer [postwalk]]
            [crux.api :as api]))

(def operators #{:$eq :$gt :$gte :$lt :$lte :$exists :$in})
(def conditions #{:$and :$not :$or :$nor})

(defn- operator? [k] (.startsWith (name k) "$"))
(def field? (complement operator?))

(defn- root? [ast]
  (and (vector? ast) (and (= :root (first ast)))))

(defn- and? [ast]
  (and (vector? ast) (and (= :condition (first ast)) (= :$and (second ast)))))

(defn ->ast [selector]
  (->> (letfn [(stepfn [selector]
                 (into []
                       (for [[k v] selector]
                         (if (conditions k)
                           [:condition k (vec (mapcat stepfn (if (vector? v) v [v])))]
                           (if (field? k)
                             (let [[op operand] (first (if (map? v) v {:$eq v}))]
                               (when-not (operators op)
                                 (throw (ex-info "Invalid Query" {:error :invalid-operator :operator op})))
                               [:field k op operand])
                             (throw (ex-info "Invalid Query" {:error :invalid-selector :operator k})))))))]
         (stepfn selector))
       (vector :root)
       (postwalk (fn [x]
                   (if (or (root? x) (and? x))
                     (conj (vec (drop-last x))
                           (reduce into [] (for [x (last x)] (if (and? x) (last x) [x]))))
                     x)))))

(def operators->datalog {:$eq '=
                         :$gt '>
                         :$gte '>=
                         :$lt '<
                         :$lte '<=})

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
        [(list (operators->datalog op) (field->vars field) literal)]))

    :condition
    (let [[_ condition args] node]
      (when-let [sub-clauses (map (partial ->where field->vars) args)]
        (case condition
          :$and
          (apply list 'and sub-clauses)
          :$not
          (apply list 'not sub-clauses)
          :$or
          (apply list 'or (ground-vars sub-clauses))
          :$nor
          (list 'not (apply list 'or (ground-vars sub-clauses))))))))

(defn ->datalog [{:keys [selector limit offset order-by lookup]}]
  (let [ast (->ast selector)
        field->vars (let [fields (atom #{})]
                      (postwalk (fn [x]
                                  (when (vector? x)
                                    (let [[t f o v] x]
                                      (when (and (= :field t)
                                                 (not (and (= :$exists o) (false? v))))
                                        (swap! fields conj f))))
                                  x)
                                ast)
                      (zipmap @fields (repeatedly gensym)))]

    (cond-> {:find (into ['e] (mapv (fn [[k]]
                                      (field->vars k))
                                    (apply merge order-by)))
             :where (into []
                          (concat [['e :crux.db/id]]
                                  (for [[field var] field->vars]
                                    ['e field var])
                                  (remove nil? (->where field->vars ast))))}

      limit
      (assoc :limit limit)

      offset
      (assoc :offset offset)

      order-by
      (assoc :order-by (mapv (fn [[k direction]]
                               [(field->vars k) direction])
                             (apply merge order-by)))

      lookup
      (as-> q1
          (let [m (into {'e 'e2}
                        (for [[k v] (:let lookup)]
                          [(keyword (str "$" (subs (str v) 1))) (get field->vars k)]))
                q2 (postwalk (fn [x] (or (m x) x)) (->datalog {:selector (:from lookup)}))]
            (merge-with (fnil into []) q1 q2))))))

(defn select [db {:keys [fields lookup] :as q}]
  (for [results (->> (doto (->datalog q) prn)
                     (api/q db)
                     (partition-by first))]
    (cond-> (api/entity db (ffirst results))
      fields
      (select-keys fields)

      lookup
      (assoc (:as lookup) (map (partial api/entity db) (map second results))))))
