(ns crux.bench.tpch-stress-test
  (:require [crux.bench :as bench]
            [crux.api :as crux]
            [clojure.tools.logging :as log]
            [crux.fixtures.tpch :as tpch]))

(defn- load-tpch-docs [node]
  (bench/run-bench :ingest
   (bench/with-additional-index-metrics node
     (tpch/load-docs! node))))

(def fields '{:l_orderkey l_orderkey
              :l_partkey l_partkey
              :l_suppkey l_suppkey
              :l_linenumber l_linenumber
              :l_quantity l_quantity
              :l_extendedprice l_extendedprice
              :l_discount l_discount
              :l_tax l_tax
              :l_returnflag l_returnflag
              :l_linestatus l_linestatus
              :l_shipdate l_shipdate
              :l_commitdate l_commitdate
              :l_receiptdate l_receiptdate
              :l_shipinstruct l_shipinstruct
              :l_shipmode l_shipmode
              :l_comment l_comment})

(defn run-stress-queries [node {:keys [query-count field-count] :or {query-count 50 field-count (count fields)} :as opts}]
  (let [q {:find '[e],
           :where (->> fields
                       (take field-count)
                       (mapcat (fn [[a v]] [['e a v] [(list 'identity v) (gensym)]]))
                       vec)
           :timeout 1000000}]
    (log/infof "Stressing query: %s" (prn-str q))
    (let [results (bench/run-bench :query-stress
                                   (dotimes [n query-count]
                                     (log/info (format "Starting query #%s" n))
                                     {:count (count (crux/q (crux/db node) q))})
                                   {:run-success? true})]
      (log/infof results)
      results)))

(defn run-tpch-stress-test [node {:keys [query-count field-count] :as opts}]
  (bench/with-bench-ns :tpch-stress
    (bench/with-crux-dimensions
      (load-tpch-docs node)
      (run-stress-queries node opts))))

(comment
  (let [node (user/crux-node)]
    (bench/with-bench-ns :tpch-stress
      (load-tpch-docs node)
      (run-stress-queries node {}))))
