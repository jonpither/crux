(ns crux.calcite-microbench-test
  (:require [clojure.test :as t]
            [crux.calcite :as cal]
            [crux.api :as c]
            [crux.fixtures :as fix :refer [*api*]]
            [crux.fixtures.calcite :as cf]
            [crux.fixtures.tpch :as tf]
            [user :as user]
            [crux.api :as crux])
  (:import io.airlift.tpch.TpchTable
           java.sql.DriverManager
           java.util.function.Supplier
           java.sql.PreparedStatement
           (org.agrona ExpandableDirectByteBuffer)))

(defn- load-docs! [node]
  (doseq [^TpchTable t (TpchTable/getTables)]
    (let [docs (tf/tpch-table->docs t)]
      (println "Transacting" (count docs) (.getTableName t))
      (fix/transact! node (tf/tpch-table->docs t)))))

(defn with-timing* [f]
  (let [start-time-ms (System/currentTimeMillis)
        ret (try
              (f)
              (catch Exception e
                {:error (.getMessage e)}))]
    (merge (when (map? ret) ret)
           {:time-taken-ms (- (System/currentTimeMillis) start-time-ms)})))

(defn ^java.sql.PreparedStatement prepared-query [^java.sql.Connection conn q]
  (.prepareStatement conn q))

(defn query [^java.sql.Connection conn q]
  (with-open [stmt (.createStatement conn)
              rs (.executeQuery stmt q)]
    (->> rs resultset-seq (into []))))

;; A Control
(defn foo [e a]
  :foo)

(defn foo2 [e a]
  (def e e)
  (def a a)
  :foo)

(comment
  (load-docs! (user/crux-node))
  (fix/transact! (user/crux-node) (tf/tpch-tables->crux-sql-schemas))
  (def db (c/db (user/crux-node)))
  (def conn (cal/jdbc-connection (user/crux-node)))
  (def p (prepared-query conn "SELECT c_name FROM CUSTOMER"))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e ?n]
                                             :where [[e :l_partkey ?n]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [l_orderkey],
                                             :where [[e :l_orderkey l_orderkey]
                                                     [e :l_partkey l_partkey]
                                                     [e :l_suppkey l_suppkey]
                                                     [e :l_linenumber l_linenumber]
                                                     [e :l_quantity l_quantity]
                                                     [e :l_extendedprice l_extendedprice]
                                                     [e :l_discount l_discount]
                                                     [e :l_tax l_tax]
                                                     [e :l_returnflag l_returnflag]
                                                     [e :l_linestatus l_linestatus]
                                                     [e :l_shipdate l_shipdate]
                                                     [e :l_commitdate l_commitdate]]
                                             :timeout 100000}))}))) ;; ~ 30 secs

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [l_orderkey, l_partkey, l_suppkey, l_linenumber],
                                             :where [[e :l_orderkey l_orderkey]
                                                     [(crux.query/optional e :l_partkey) l_partkey]
                                                     [(crux.query/optional e :l_suppkey) l_suppkey]
                                                     [(crux.query/optional e :l_linenumber) l_linenumber]
                                                     [(crux.query/optional e :l_quantity) l_quantity40]
                                                     [(crux.query/optional e :l_extendedprice) l_extendedprice]
                                                     [(crux.query/optional e :l_discount) l_discount]
                                                     [(crux.query/optional e :l_tax) l_tax]
                                                     [(crux.query/optional e :l_returnflag) l_returnflag]
                                                     [(crux.query/optional e :l_linestatus) l_linestatus]
                                                     [(crux.query/optional e :l_shipdate) l_shipdate]
                                                     [(crux.query/optional e :l_commitdate) l_commitdate]]

                                             :timeout 100000}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [l_orderkey],
                                             :where [[e :l_orderkey l_orderkey]
                                                     [(crux.calcite-microbench-test/foo e :l_partkey) l_partkey]
                                                     [(crux.calcite-microbench-test/foo e :l_suppkey) l_suppkey]
                                                     [(crux.calcite-microbench-test/foo e :l_linenumber) l_linenumber]
                                                     [(crux.calcite-microbench-test/foo e :l_quantity) l_quantity2]
                                                     ;; [(crux.calcite-microbench-test/foo e :l_extendedprice) l_extendedprice]
                                                     ;; [(crux.calcite-microbench-test/foo e :l_discount) l_discount]
                                                     ;; [(crux.calcite-microbench-test/foo e :l_tax) l_tax]
                                                     ;; [(crux.calcite-microbench-test/foo e :l_returnflag) l_returnflag]
                                                     ]

                                             :timeout 100000}))})))

  ;; query -> build-sub-query -> compile-sub-query -> build-pred-constraints
  ;; Decision at lambda time, or query execution time?. Has to be lambda time
  ;; Er on the side of minimal to make work

  (def ^:private ^ThreadLocal e-buffer-tl
    (ThreadLocal/withInitial
     (reify Supplier
       (get [_]
         (ExpandableDirectByteBuffer.)))))

  (def ^:private ^ThreadLocal a-buffer-tl
    (ThreadLocal/withInitial
     (reify Supplier
       (get [_]
         (ExpandableDirectByteBuffer.)))))

  (defn optionalg [e a]
    (let [db crux.query/*db*
          index-store crux.query/*index-store*]
      (crux.db/decode-value index-store (first (crux.db/aev index-store
                                                            (crux.codec/->id-buffer a a-buffer-tl)
                                                            (crux.codec/->id-buffer e e-buffer-tl)
                                                            nil
                                                            (:entity-resolver-fn db))))))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e ?c_name]
                                             :where [[e :c_acctbal ?bal]
                                                     [(crux.calcite-microbench-test/optionale e :c_name) ?c_name]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e],
                                             :where [[e :c_custkey c_custkey]
                                                     [e :c_name c_name]
                                                     [e :c_address c_address]
                                                     [e :c_nationkey c_nationkey]
                                                     [e :c_phone c_phone]
                                                     [e :c_acctbal c_acctbal]
                                                     [e :c_mktsegment c_mktsegment]
                                                     [e :c_comment c_comment]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e],
                                             :where [[e :c_custkey c_custkey]
                                                     [(crux.calcite-microbench-test/optionale e :c_name) c_name]
                                                     [(crux.calcite-microbench-test/optionale e :c_address) c_address]
                                                     [(crux.calcite-microbench-test/optionale e :c_nationkey) c_nationkey]
                                                     [(crux.calcite-microbench-test/optionale e :c_phone) c_phone]
                                                     [(crux.calcite-microbench-test/optionale e :c_acctbal) c_acctbal]
                                                     [(crux.calcite-microbench-test/optionale e :c_mktsegment) c_mktsegment]
                                                     [(crux.calcite-microbench-test/optionale e :c_comment) c_comment]]}))})))

  (defprotocol Bob
    (foo [a]))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :c_acctbal ?bal0]
                                                     [e :c_name ?c_name7]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :c_acctbal ?bal0]
                                                     [(crux.query/optional e :c_name) ?c_name1]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :c_acctbal ?bal0]
                                                     [(crux.calcite-microbench-test/foo2 e :c_name) ?c_namef]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :c_acctbal ?bal2]
                                                     [e :c_name ?c_name]]}))})))

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e],
                                             :where [[e :c_custkey c_custkey]
                                                     [(crux.calcite-microbench-test/foo e :c_name) c_name]
                                                     [(crux.calcite-microbench-test/foo e :c_address) c_address]
                                                     [(crux.calcite-microbench-test/foo e :c_nationkey) c_nationkey]
                                                     [(crux.calcite-microbench-test/foo e :c_phone) c_phone]
                                                     [(crux.calcite-microbench-test/foo e :c_acctbal) c_acctbal]
                                                     [(crux.calcite-microbench-test/foo e :c_mktsegment) c_mktsegment]
                                                     [(crux.calcite-microbench-test/foo e :c_comment) c_comment]]}))})))


  (println (with-timing*
             (fn [] {:count (first (query conn "SELECT l_orderkey FROM LINEITEM"))})))

  (println (with-timing*
             (fn [] {:count (count (cf/exec-prepared-query
                                    (let [s (prepared-query conn "SELECT * FROM LINEITEM LIMIT 10")]
                                      (.setQueryTimeout s 10)
                                      s)))})))

  ;; Testing vars for range searches

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :l_quantity ?qty]
                                                     [(> ?qty qty)]]
                                             :args [{:qty 30.0}]}))})))

  ;; using args:

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :l_quantity qty]]
                                             :args [{:qty 30.0}]}))})))
  ;; Args penalises
  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :l_quantity ?qty]
                                                     [(= qty ?qty)]]
                                             :args [{:qty 30.0}]}))})))

  ;; Not using Args (below)

  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :l_quantity 30.0]]}))})))

  ;; Fast:
  (println (with-timing*
             (fn [] {:count (count (c/q db '{:find [e]
                                             :where [[e :l_quantity ?qty]
                                                     [(= ?qty 30)]]}))})))

  ;; Statement - = is optimised for, except when used with args

  ;; SQL (below)

  (println (with-timing*
             (fn [] {:count (count (query conn "SELECT * FROM LINEITEM WHERE L_QUANTITY = 30.0"))})))

  (println (with-timing*
             (fn [] {:count (count (query conn "SELECT * FROM CUSTOMER"))})))

  (println (with-timing*
             (fn [] {:count (count (cf/exec-prepared-query (prepared-query conn "SELECT c_name FROM CUSTOMER")))})))

  (println (with-timing*
             (fn [] {:count (count (cf/exec-prepared-query p))}))))
