(ns crux.vega)

(def timers (atom {}))

(defn ->console []
  (doseq [[k v] (sort-by val @timers)]
    (prn (str "Elapsed time " k ": " (/ v 1000000.0) " msecs"))))

(defn add-time [k t]
  (swap! crux.vega/timers update k #(+ (or ^double % 0) t)))

(defmacro vega-time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [name expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (add-time ~name (double (- (. System (nanoTime)) start#)))
     ret#))

(comment
  (vega-time :a (+ 1 1)))
