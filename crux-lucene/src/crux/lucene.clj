(ns crux.lucene
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [crux.bus :as bus]
            [crux.codec :as cc]
            [crux.db :as db]
            [crux.io :as cio]
            [crux.query :as q]
            [crux.system :as sys])
  (:import crux.query.VarBinding
           java.io.Closeable
           java.nio.file.Path
           org.apache.lucene.analysis.Analyzer
           org.apache.lucene.analysis.standard.StandardAnalyzer
           [org.apache.lucene.document Document Field Field$Store StoredField StringField TextField]
           [org.apache.lucene.index DirectoryReader IndexWriter IndexWriterConfig Term]
           org.apache.lucene.queries.function.FunctionScoreQuery
           org.apache.lucene.queryparser.classic.QueryParser
           [org.apache.lucene.search BooleanClause$Occur BooleanQuery$Builder DoubleValuesSource IndexSearcher Query ScoreDoc TermQuery]
           [org.apache.lucene.store Directory FSDirectory]))

(def ^:dynamic *lucene-store*)

(defrecord LuceneNode [directory analyzer indexer]
  java.io.Closeable
  (close [this]
    (cio/try-close directory)))

(defn- ^String ->hash-str [eid]
  (str (cc/new-id eid)))

(defrecord DocumentId [a v])

(defn ^String keyword->k [k]
  (subs (str k) 1))

(def ^:const ^:private field-crux-id "_crux_id")
(def ^:const ^:private field-crux-val "_crux_val")
(def ^:const ^:private field-crux-attr "_crux_attr")

(defn doc-count []
  (let [{:keys [^Directory directory]} *lucene-store*
        directory-reader (DirectoryReader/open directory)]
    (.numDocs directory-reader)))

(defn- ^IndexWriter index-writer [lucene-store]
  (let [{:keys [^Directory directory ^Analyzer analyzer]} lucene-store]
    (IndexWriter. directory, (IndexWriterConfig. analyzer))))

(defn- index-tx! [^IndexWriter index-writer tx]
  (let [t (Term. "meta" "latest-completed-tx")
        d (doto (Document.)
            (.add (StringField. "meta", "latest-completed-tx" Field$Store/NO))
            (.add (StoredField. "latest-completed-tx" ^long (:crux.tx/tx-id tx))))]
    (.updateDocument index-writer t d)))

(defn latest-submitted-tx [lucene-store]
  (let [{:keys [^Directory directory]} lucene-store]
    (when (DirectoryReader/indexExists directory)
      (with-open [directory-reader (DirectoryReader/open directory)]
        (let [index-searcher (IndexSearcher. directory-reader)
              q (TermQuery. (Term. "meta" "latest-completed-tx"))
              d ^ScoreDoc (first (.-scoreDocs (.search index-searcher q 1)))]
          (when d
            (some-> (.doc index-searcher (.-doc d))
                    (.get "latest-completed-tx")
                    (Long/parseLong))))))))

(defn validate-lucene-store-up-to-date [index-store lucene-store]
  (let [{:crux.tx/keys [tx-id] :as latest-tx} (db/latest-completed-tx index-store)
        latest-lucene-tx-id (latest-submitted-tx lucene-store)]
    (when (and tx-id
               (or (nil? latest-lucene-tx-id)
                   (> tx-id latest-lucene-tx-id)))
      (throw (IllegalStateException. "Lucene store latest tx mismatch")))))

(defn search [lucene-store, ^Query q]
  (assert lucene-store)
  (let [{:keys [^Directory directory ^Analyzer analyzer]} lucene-store
        directory-reader (DirectoryReader/open directory)
        index-searcher (IndexSearcher. directory-reader)
        q (FunctionScoreQuery. q (DoubleValuesSource/fromQuery q))
        score-docs (.-scoreDocs (.search index-searcher q 1000))]

    (when (seq score-docs)
      (log/debug (.explain index-searcher q (.-doc ^ScoreDoc (first score-docs)))))

    (cio/->cursor (fn []
                    (.close directory-reader))
                  (map (fn [^ScoreDoc d]
                         (vector (.doc index-searcher (.-doc d))
                                 (.-score d))) score-docs))))

(defn pred-constraint [query-builder results-resolver {:keys [arg-bindings encode-value-fn idx-id return-type tuple-idxs-in-join-order]}]
  (fn pred-get-attr-constraint [index-snapshot db idx-id->idx join-keys]
    (let [arg-bindings (map (fn [a]
                              (if (instance? VarBinding a)
                                (q/bound-result-for-var index-snapshot a join-keys)
                                a))
                            (rest arg-bindings))
          query (query-builder (:analyzer *lucene-store*) arg-bindings)
          tuples (with-open [search-results ^crux.api.ICursor (search *lucene-store* query)]
                   (->> search-results
                        iterator-seq
                        (results-resolver index-snapshot db)
                        (into [])))]
      (q/bind-binding return-type tuple-idxs-in-join-order (get idx-id->idx idx-id) tuples))))

(defn ^Query build-query
  "Standard build query fn, taking a single field/val lucene term string."
  [^Analyzer analyzer, [k v]]
  (when-not (string? v)
    (throw (IllegalArgumentException. "Lucene text search values must be String")))
  (let [qp (QueryParser. (keyword->k k) analyzer)
        b (doto (BooleanQuery$Builder.)
            (.add (.parse qp v) BooleanClause$Occur/MUST))]
    (.build b)))

(defn resolve-search-results-a-v
  "Given search results each containing a single A/V pair document,
  perform a temporal resolution against A/V to resolve the eid."
  [attr index-snapshot {:keys [entity-resolver-fn] :as db} search-results]
  (mapcat (fn [[^Document doc score]]
            (let [v (.get ^Document doc field-crux-val)]
              (for [eid (db/ave index-snapshot attr v nil entity-resolver-fn)]
                [(db/decode-value index-snapshot eid) v score])))
          search-results))

(defmethod q/pred-args-spec 'text-search [_]
  (s/cat :pred-fn  #{'text-search} :args (s/spec (s/cat :attr keyword? :v (some-fn string? symbol?))) :return (s/? :crux.query/binding)))

(defmethod q/pred-constraint 'text-search [_ pred-ctx]
  (let [resolver (partial resolve-search-results-a-v (second (:arg-bindings pred-ctx)))]
    (pred-constraint #'build-query resolver pred-ctx)))

(defn- resolve-search-results-a-v-wildcard
  "Given search results each containing a single A/V pair document,
  perform a temporal resolution against A/V to resolve the eid."
  [index-snapshot {:keys [entity-resolver-fn] :as db} search-results]
  (mapcat (fn [[^Document doc score]]
            (let [v (.get ^Document doc field-crux-val)
                  a (keyword (.get ^Document doc field-crux-attr))]
              (for [eid (db/ave index-snapshot a v nil entity-resolver-fn)]
                [(db/decode-value index-snapshot eid) v a score])))
          search-results))

(defn ^Query build-query-wildcard
  "Wildcard query builder"
  [^Analyzer analyzer, [v]]
  (when-not (string? v)
    (throw (IllegalArgumentException. "Lucene text search values must be String")))
  (let [qp (QueryParser. field-crux-val analyzer)
        b (doto (BooleanQuery$Builder.)
            (.add (.parse qp v) BooleanClause$Occur/MUST))]
    (.build b)))

(defmethod q/pred-args-spec 'wildcard-text-search [_]
  (s/cat :pred-fn #{'wildcard-text-search} :args (s/spec (s/cat :v string?)) :return (s/? :crux.query/binding)))

(defmethod q/pred-constraint 'wildcard-text-search [_ pred-ctx]
  (pred-constraint #'build-query-wildcard #'resolve-search-results-a-v-wildcard pred-ctx))

(defprotocol LuceneIndexer
  (index! [this index-writer docs])
  (evict! [this index-writer eids]))

(defrecord LuceneAvIndexer [index-store]
  LuceneIndexer

  (index! [this index-writer docs]
    (doseq [crux-doc (vals docs)
            [k v] (->> (dissoc crux-doc :crux.db/id)
                       (mapcat (fn [[k v]]
                                 (for [v (cc/vectorize-value v)
                                       :when (string? v)]
                                   [k v]))))
            :let [id-str (->hash-str (DocumentId. k v))
                  doc (doto (Document.)
                        ;; To search for triples by a-v for deduping
                        (.add (StringField. field-crux-id, id-str, Field$Store/NO))
                        ;; The actual term, which will be tokenized
                        (.add (TextField. (keyword->k k), v, Field$Store/YES))
                        ;; Used for wildcard searches
                        (.add (TextField. field-crux-val, v, Field$Store/YES))
                        ;; Used for wildcard searches
                        (.add (StringField. field-crux-attr, (keyword->k k), Field$Store/YES)))]]
      (.updateDocument ^IndexWriter index-writer (Term. field-crux-id id-str) doc)))

  (evict! [this index-writer eids]
    (let [attrs-id->attr (->> (db/read-index-meta index-store :crux/attribute-stats)
                              keys
                              (map #(vector (->hash-str %) %))
                              (into {}))]
      (with-open [index-snapshot (db/open-index-snapshot index-store)]
        (let [qs (for [[a v] (db/exclusive-avs index-store eids)
                       :let [a (attrs-id->attr (->hash-str a))
                             v (db/decode-value index-snapshot v)]
                       :when (not= :crux.db/id a)]
                   (TermQuery. (Term. field-crux-id (->hash-str (DocumentId. a v)))))]
          (.deleteDocuments ^IndexWriter index-writer ^"[Lorg.apache.lucene.search.Query;" (into-array Query qs)))))))

(defn ->indexer
  {::sys/deps {:index-store :crux/index-store}}
  [{:keys [index-store]}]
  (LuceneAvIndexer. index-store))

(defn ->analyzer
  [_]
  (StandardAnalyzer.))

(defn ->lucene-store
  {::sys/args {:db-dir {:doc "Lucene DB Dir"
                        :required? true
                        :spec ::sys/path}}
   ::sys/deps {:bus :crux/bus
               :document-store :crux/document-store
               :index-store :crux/index-store
               :indexer ::indexer
               :analyzer ::analyzer}}
  [{:keys [^Path db-dir index-store document-store bus analyzer indexer] :as opts}]
  (let [directory (FSDirectory/open db-dir)
        lucene-store (LuceneNode. directory analyzer indexer)]
    (validate-lucene-store-up-to-date index-store lucene-store)
    (alter-var-root #'*lucene-store* (constantly lucene-store))
    (bus/listen bus {:crux/event-types #{:crux.tx/indexing-tx-pre-commit :crux.tx/indexed-docs :crux.tx/unindexing-eids}
                     :crux.bus/executor (reify java.util.concurrent.Executor
                                          (execute [_ f]
                                            (.run f)))}
                (fn [ev]
                  (with-open [index-writer (index-writer lucene-store)]
                    (case (:crux/event-type ev)

                      :crux.tx/indexing-tx-pre-commit
                      (index-tx! index-writer (:crux.tx/submitted-tx ev))

                      :crux.tx/indexed-docs
                      (index! indexer index-writer (db/fetch-docs document-store (:doc-ids ev)))

                      :crux.tx/unindexing-eids
                      (evict! indexer index-writer (:eids ev))))))
    lucene-store))
