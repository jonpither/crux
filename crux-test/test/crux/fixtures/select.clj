(ns crux.fixtures.select
  (:require [crux.api :as api]
            [crux.fixtures :as fix :refer [*api*]]
            crux.select))

(defn select [q & [{:as opts}]]
  (crux.select/select (api/db *api*) (merge opts {:selector q})))

(defmacro ex-data-thrown? [d & form]
  `(try
     ~@form
     (t/is false)
     (catch clojure.lang.ExceptionInfo t#
       (t/is (= ~d (.getData t#))))))
