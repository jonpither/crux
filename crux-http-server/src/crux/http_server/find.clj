(ns crux.http-server.find
  (:require [clojure.spec.alpha :as s]
            [crux.http-server.json :as http-json]
            [crux.http-server.util :as util]
            [crux.select :as cs]
            [jsonista.core :as json]
            [muuntaja.core :as m]
            [muuntaja.format.core :as mfc]
            [spec-tools.core :as st]))

(defn- ->find-json-decoder [_]
  (reify
    mfc/Decode
    (decode [_ data _]
      (json/read-value data http-json/crux-object-mapper))))

(def ->find-muuntaja
  (m/create
   (assoc-in (util/->default-muuntaja {:json-encode-fn http-json/camel-case-keys})
             [:formats "application/json" :decoder]
             [->find-json-decoder])))

(s/def ::selector
  (st/spec
   {:spec any?
    :swagger/example 'stuff
    :description "Selector"}))

(s/def ::body-params
  (s/keys :req-un [::selector]))

(s/def ::query-params
  (s/keys :opt-un [::util/valid-time ::util/tx-time]))

(defn run-find [find-expr {:keys [crux-node valid-time tx-time]}]
  (let [db (util/db-for-request crux-node {:valid-time valid-time
                                           :tx-time tx-time})]
    (println "Valid time" valid-time)
    (cs/select db find-expr)))

(defn find-handler [options]
  (fn [req]
    (let [{query-params :query body :body} (get-in req [:parameters])
          {:keys [valid-time tx-time]} query-params]
      {:status 200
       :body (run-find body
                       (assoc options
                              :valid-time valid-time
                              :tx-time tx-time))})))
