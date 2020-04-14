(ns crux.fixtures.tpch
  (:import [io.airlift.tpch TpchColumn TpchColumnType TpchColumnType$Base TpchEntity TpchTable]))

(def tpch-column-types->crux-calcite-type
  {TpchColumnType$Base/INTEGER :integer
   TpchColumnType$Base/VARCHAR :varchar
   TpchColumnType$Base/IDENTIFIER :long
   TpchColumnType$Base/DOUBLE :double
   TpchColumnType$Base/DATE :datetime})

(defn tpch-table->crux-sql-schema [^TpchTable t]
  {:crux.db/id (keyword "crux.sql.schema" (.getTableName t))
   :crux.sql.table/name (.getTableName t)
   :crux.sql.table/query {:find (vec (for [^TpchColumn c (.getColumns t)]
                                       (symbol (.getColumnName c))))
                          :where (vec (for [^TpchColumn c (.getColumns t)]
                                        ['e (keyword (.getSimplifiedColumnName c)) (symbol (.getColumnName c))]))}
   :crux.sql.table/columns (into {} (for [^TpchColumn c (.getColumns t)]
                                      [(symbol (.getColumnName c)) (tpch-column-types->crux-calcite-type (.getBase (.getType c)))]))})

(defn tpch-tables->crux-sql-schemas []
  (map tpch-table->crux-sql-schema (TpchTable/getTables)))

(defn tpch-entity->doc [^TpchTable t ^TpchEntity b]
  (into {:crux.db/id (keyword (str (.getTableName t) "-" (.getRowNumber b)))}
        (for [^TpchColumn c (.getColumns t)]
          [(keyword (.getSimplifiedColumnName c))
           (condp = (.getBase (.getType c))
             TpchColumnType$Base/IDENTIFIER
             (.getIdentifier c b)
             TpchColumnType$Base/INTEGER
             (.getInteger c b)
             TpchColumnType$Base/VARCHAR
             (.getString c b)
             TpchColumnType$Base/DOUBLE
             (.getDouble c b)
             TpchColumnType$Base/DATE
             (.getDate c b))])))

(defn tpch-table->docs [^TpchTable t]
  ;; first happens to be customers (;; 150000)
  (map (partial tpch-entity->doc t) (seq (.createGenerator ^TpchTable t 0.005 1 1))))

(comment
  (first (tpch-tables->crux-sql-schemas))
  {:crux.db/id :crux.sql.schema/customer,
   :crux.sql.table/name "customer",
   :crux.sql.table/columns [{:crux.sql.column/attribute :c_custkey, :crux.sql.column/name "c_custkey", :crux.sql.column/type :varchar}
                            {:crux.sql.column/attribute :c_name, :crux.sql.column/name "c_name", :crux.sql.column/type :varchar}
                            {:crux.sql.column/attribute :c_address, :crux.sql.column/name "c_address", :crux.sql.column/type :varchar}
                            {:crux.sql.column/attribute :c_nationkey, :crux.sql.column/name "c_nationkey", :crux.sql.column/type :varchar}
                            {:crux.sql.column/attribute :c_phone, :crux.sql.column/name "c_phone", :crux.sql.column/type :varchar}
                            {:crux.sql.column/attribute :c_acctbal, :crux.sql.column/name "c_acctbal", :crux.sql.column/type :double}
                            {:crux.sql.column/attribute :c_mktsegment, :crux.sql.column/name "c_mktsegment", :crux.sql.column/type :varchar}
                            {:crux.sql.column/attribute :c_comment, :crux.sql.column/name "c_comment", :crux.sql.column/type :varchar}]}

  (first (tpch-table->docs (first (TpchTable/getTables)) 5)))
