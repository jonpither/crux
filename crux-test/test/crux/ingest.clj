(ns ingest
  (:require [criterium.core :as crt]
            [crux.api :as crux]
            [crux.fixtures :as fix :refer [*api*]]))

(comment
  (do
    (reset! crux.vega/timers {})
    (crux.vega/vega-time
     :total-run
     (let [node (dev/crux-node)]
       (dotimes [n 25]
         (fix/submit+await-tx node (for [doc-id (range 1000)]
                                     [:crux.tx/put {:crux.db/id (+ (* n 1000) doc-id)}]))
         (crux/sync node))))
    (crux.vega/->console)))

(comment
  (crt/with-progress-reporting
    (crt/quick-bench
     (let [n 10
           node (dev/crux-node)]
       (do
         (when-not (crux/latest-submitted-tx node)
           (fix/submit+await-tx node (for [doc-id (range n)]
                                       [:crux.tx/put {:crux.db/id doc-id}])))
         (crux/sync node))))))
