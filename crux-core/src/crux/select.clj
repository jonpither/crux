(ns crux.select
  (:require [clojure.walk :refer [postwalk]]
            [crux.api :as api]))

(def operators #{:$eq :$gt :$gte :$lt :$lte :$exists :$in})
(def conditions #{:$and :$not :$or :$nor})

(defn- operator? [k] (.startsWith (name k) "$"))
(def field? (complement operator?))

(defn ->ast [selector]
  (into []
        (for [[k v] selector]
          (if (conditions k)
            [:condition k (vec (mapcat ->ast (if (vector? v) v [v])))]
            (if (field? k)
              (let [[op operand] (first (if (map? v) v {:$eq v}))]
                (when-not (operators op)
                  (throw (ex-info "Invalid Query" {:error :invalid-operator :operator op})))
                [:field k op operand])
              (throw (ex-info "Invalid Query" {:error :invalid-selector :operator k})))))))

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
      (case op
        :$exists
        (when (not literal)
          (list 'not ['e field]))

        :$in
        [(list '== (field->vars field) (set literal))]

        ;; default
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

(defn- unpack-nested-and [datalog]
  (letfn [(unpack [x] (reduce into [] (for [x x] (if (= 'and (first x)) (rest x) [x]))))]
    (clojure.walk/postwalk (fn [x]
                             (if (and (map? x) (:where x))
                               (update x :where unpack)
                               (if (and (list? x) (= 'and (first x)))
                                 (apply list 'and (unpack (rest x)))
                                 x)))
                           datalog)))

(defn ->datalog [{:keys [selector limit offset order-by lookup]}]
  (let [ast [:root (->ast selector)]
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
    (cond-> {}

      true
      (assoc :find (into ['e] (mapv (fn [[k]]
                                      (field->vars k))
                                    (apply merge order-by))))

      true
      (assoc :where (into []
                          (concat [['e :crux.db/id]]
                                  (for [[field var] field->vars]
                                    ['e field var])
                                  (remove nil? (->where field->vars ast)))))

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
          (let [q-let-vars (for [[k v] (:let lookup)]
                             [k (keyword (str "$" (subs (str v) 1))) (gensym)])
                q2-var-swap (merge {'e 'e2}
                                   (into {}
                                         (for [[_ v s] q-let-vars]
                                           [v s])))
                q2 (->> (->datalog {:selector (:from lookup)})
                        (postwalk (fn [x] (or (q2-var-swap x) x))))]
            (merge-with (fnil into []) q1 q2 {:where (vec (for [[k _ s] q-let-vars]
                                                            ['e k s]))})))

      true
      (unpack-nested-and))))

(defn select [db {:keys [fields lookup] :as q}]
  (for [results (->> (doto (->datalog q) prn)
                     (api/q db)
                     (partition-by first))]
    (cond-> (api/entity db (ffirst results))
      fields
      (select-keys fields)

      lookup
      (assoc (:as lookup) (map (partial api/entity db) (map second results))))))
