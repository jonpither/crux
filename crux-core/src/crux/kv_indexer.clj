(ns crux.kv-indexer
  (:require [crux.codec :as c]
            [crux.db :as db]
            [crux.index :as idx]
            [crux.io :as cio]
            [crux.kv :as kv]
            [crux.lru :as lru]
            [crux.memory :as mem]
            [crux.status :as status]
            [crux.morton :as morton])
  (:import (crux.codec Id EntityTx)
           java.io.Closeable
           java.util.function.Supplier
           (clojure.lang MapEntry)
           (org.agrona DirectBuffer MutableDirectBuffer ExpandableDirectByteBuffer)))

(set! *unchecked-math* :warn-on-boxed)

(defn etx->kvs [^EntityTx etx]
  [[(c/encode-entity+vt+tt+tx-id-key-to
     nil
     (c/->id-buffer (.eid etx))
     (.vt etx)
     (.tt etx)
     (.tx-id etx))
    (c/->id-buffer (.content-hash etx))]
   [(c/encode-entity+z+tx-id-key-to
     nil
     (c/->id-buffer (.eid etx))
     (c/encode-entity-tx-z-number (.vt etx) (.tt etx))
     (.tx-id etx))
    (c/->id-buffer (.content-hash etx))]])

(def ^:private ^ThreadLocal value-buffer-tl
  (ThreadLocal/withInitial
   (reify Supplier
     (get [_]
       (ExpandableDirectByteBuffer.)))))

(defrecord PrefixKvIterator [i ^DirectBuffer prefix]
  kv/KvIterator
  (seek [_ k]
    (when-let [k (kv/seek i k)]
      (when (mem/buffers=? k prefix (.capacity prefix))
        k)))

  (next [_]
    (when-let [k (kv/next i)]
      (when (mem/buffers=? k prefix (.capacity prefix))
        k)))

  (value [_]
    (kv/value i))

  Closeable
  (close [_]
    (.close ^Closeable i)))

(defn new-prefix-kv-iterator ^java.io.Closeable [i prefix]
  (->PrefixKvIterator i prefix))

(defn all-keys-in-prefix
  ([i prefix] (all-keys-in-prefix i prefix prefix {}))
  ([i prefix seek-k] (all-keys-in-prefix i prefix seek-k {}))
  ([i ^DirectBuffer prefix, ^DirectBuffer seek-k, {:keys [entries? reverse?]}]
   (letfn [(step [k]
             (lazy-seq
              (when (and k (mem/buffers=? prefix k (.capacity prefix)))
                (cons (if entries?
                        (MapEntry/create (mem/copy-to-unpooled-buffer k) (mem/copy-to-unpooled-buffer (kv/value i)))
                        (mem/copy-to-unpooled-buffer k))
                      (step (if reverse? (kv/prev i) (kv/next i)))))))]
     (step (if reverse?
             (when (kv/seek i (-> seek-k (mem/copy-buffer) (mem/inc-unsigned-buffer!)))
               (kv/prev i))
             (kv/seek i seek-k))))))

(defn- buffer-or-value-buffer [v]
  (cond
    (instance? DirectBuffer v)
    v

    (some? v)
    (c/->value-buffer v)

    :else
    c/empty-buffer))

(defn- buffer-or-id-buffer [v]
  (cond
    (instance? DirectBuffer v)
    v

    (some? v)
    (c/->id-buffer v)

    :else
    c/empty-buffer))

(defn- inc-unsigned-prefix-buffer [buffer prefix-size]
  (mem/inc-unsigned-buffer! (mem/limit-buffer (mem/copy-buffer buffer prefix-size (.get idx/seek-buffer-tl)) prefix-size)))

(defn ^EntityTx enrich-entity-tx [entity-tx ^DirectBuffer content-hash]
  (assoc entity-tx :content-hash (when (pos? (.capacity content-hash))
                                   (c/safe-id (c/new-id content-hash)))))

(defn safe-entity-tx ^crux.codec.EntityTx [entity-tx]
  (-> entity-tx
      (update :eid c/safe-id)
      (update :content-hash c/safe-id)))

;;;; Content indices

(defrecord EntityValue [eid value])
(defrecord EntityValueContentHash [eid value content-hash])

(defn encode-ave-key-to
  (^org.agrona.MutableDirectBuffer[b attr]
   (encode-ave-key-to b attr c/empty-buffer c/empty-buffer))
  (^org.agrona.MutableDirectBuffer[b attr v]
   (encode-ave-key-to b attr v c/empty-buffer))
  (^org.agrona.MutableDirectBuffer
   [^MutableDirectBuffer b ^DirectBuffer attr ^DirectBuffer v ^DirectBuffer entity]
   (assert (= c/id-size (.capacity attr)) (mem/buffer->hex attr))
   (assert (or (= c/id-size (.capacity entity))
               (zero? (.capacity entity))) (mem/buffer->hex entity))
   (let [^MutableDirectBuffer b (or b (mem/allocate-buffer (+ c/index-id-size c/id-size (.capacity v) (.capacity entity))))]
     (mem/limit-buffer
      (doto b
        (.putByte 0 c/ave-index-id)
        (.putBytes c/index-id-size attr 0 c/id-size)
        (.putBytes (+ c/index-id-size c/id-size) v 0 (.capacity v))
        (.putBytes (+ c/index-id-size c/id-size (.capacity v)) entity 0 (.capacity entity)))
      (+ c/index-id-size c/id-size (.capacity v) (.capacity entity))))))

(defn decode-ave-key->ev-from
  ^crux.kv_indexer.EntityValue [^DirectBuffer k]
  (let [length (long (.capacity k))]
    (assert (<= (+ c/index-id-size c/id-size c/id-size) length) (mem/buffer->hex k))
    (let [index-id (.getByte k 0)]
      (assert (= c/ave-index-id index-id))
      (let [value-size (- length c/id-size c/id-size c/index-id-size)
            value (mem/slice-buffer k (+ c/index-id-size c/id-size) value-size)
            entity (Id. (mem/slice-buffer k (+ c/index-id-size c/id-size value-size) c/id-size) 0)]
        (->EntityValue entity value)))))

(defn encode-aecv-key-to
  (^org.agrona.MutableDirectBuffer [b attr]
   (encode-aecv-key-to b attr c/empty-buffer c/empty-buffer c/empty-buffer))
  (^org.agrona.MutableDirectBuffer [b attr entity]
   (encode-aecv-key-to b attr entity c/empty-buffer c/empty-buffer))
  (^org.agrona.MutableDirectBuffer [b attr entity content-hash]
   (encode-aecv-key-to b attr entity content-hash c/empty-buffer))
  (^org.agrona.MutableDirectBuffer [^MutableDirectBuffer b ^DirectBuffer attr ^DirectBuffer entity ^DirectBuffer content-hash ^DirectBuffer v]
   (assert (= c/id-size (.capacity attr)) (mem/buffer->hex attr))
   (assert (or (= c/id-size (.capacity content-hash))
               (zero? (.capacity content-hash))) (mem/buffer->hex content-hash))
   (assert (or (= c/id-size (.capacity entity))
               (zero? (.capacity entity))) (mem/buffer->hex entity))
   (let [^MutableDirectBuffer b (or b (mem/allocate-buffer (+ c/index-id-size c/id-size (.capacity entity) (.capacity content-hash) (.capacity v))))]
     (-> (doto b
           (.putByte 0 c/aecv-index-id)
           (.putBytes c/index-id-size attr 0 c/id-size)
           (.putBytes (+ c/index-id-size c/id-size) entity 0 (.capacity entity))
           (.putBytes (+ c/index-id-size c/id-size (.capacity entity)) content-hash 0 (.capacity content-hash))
           (.putBytes (+ c/index-id-size c/id-size (.capacity entity) (.capacity content-hash)) v 0 (.capacity v)))
         (mem/limit-buffer (+ c/index-id-size c/id-size (.capacity entity) (.capacity content-hash) (.capacity v)))))))

(defn decode-aecv-key->evc-from ^crux.kv_indexer.EntityValueContentHash [^DirectBuffer k]
  (let [length (long (.capacity k))]
    (assert (<= (+ c/index-id-size c/id-size c/id-size) length) (mem/buffer->hex k))
    (let [index-id (.getByte k 0)]
      (assert (= c/aecv-index-id index-id))
      (let [value-size (- length c/id-size c/id-size c/id-size c/index-id-size)
            entity (Id. (mem/slice-buffer k (+ c/index-id-size c/id-size) c/id-size) 0)
            content-hash (Id. (mem/slice-buffer k (+ c/index-id-size c/id-size c/id-size) c/id-size) 0)
            value (mem/slice-buffer k (+ c/index-id-size c/id-size c/id-size c/id-size) value-size)]
        (->EntityValueContentHash entity value content-hash)))))

(defn encode-hash-cache-key-to
  (^org.agrona.MutableDirectBuffer [b entity]
   (encode-aecv-key-to b entity c/empty-buffer))

  (^org.agrona.MutableDirectBuffer [^MutableDirectBuffer b ^DirectBuffer entity ^DirectBuffer value]
   (assert (= c/id-size (.capacity entity)) (mem/buffer->hex entity))

   (let [^MutableDirectBuffer b (or b (mem/allocate-buffer (+ c/index-id-size c/id-size (.capacity value))))]
     (-> (doto b
           (.putByte 0 c/hash-cache-index-id)
           (.putBytes c/index-id-size entity 0 c/id-size)
           (.putBytes (+ c/index-id-size c/id-size) value 0 (.capacity value)))
         (mem/limit-buffer (+ c/index-id-size c/id-size (.capacity value)))))))

;;;; Entity as-of

(defn- find-first-entity-tx-within-range [i min max eid]
  (let [prefix-size (+ c/index-id-size c/id-size)
        seek-k (c/encode-entity+z+tx-id-key-to
                (.get idx/seek-buffer-tl)
                eid
                min)]
    (loop [k (kv/seek i seek-k)]
      (when (and k (mem/buffers=? seek-k k prefix-size))
        (let [z (c/decode-entity+z+tx-id-key-as-z-number-from k)]
          (if (morton/morton-number-within-range? min max z)
            (let [entity-tx (safe-entity-tx (c/decode-entity+z+tx-id-key-from k))
                  v (kv/value i)]
              (if-not (mem/buffers=? c/nil-id-buffer v)
                [(c/->id-buffer (.eid entity-tx))
                 (enrich-entity-tx entity-tx v)
                 z]
                [::deleted-entity entity-tx z]))
            (let [[litmax bigmin] (morton/morton-range-search min max z)]
              (when-not (neg? (.compareTo ^Comparable bigmin z))
                (recur (kv/seek i (c/encode-entity+z+tx-id-key-to
                                   (.get idx/seek-buffer-tl)
                                   eid
                                   bigmin)))))))))))

(defn- find-entity-tx-within-range-with-highest-valid-time [i min max eid prev-candidate]
  (if-let [[_ ^EntityTx entity-tx z :as candidate] (find-first-entity-tx-within-range i min max eid)]
    (let [[^long x ^long y] (morton/morton-number->longs z)
          min-x (long (first (morton/morton-number->longs min)))
          max-x (dec x)]
      (if (and (not (pos? (Long/compareUnsigned min-x max-x)))
               (not= y -1))
        (let [min (morton/longs->morton-number
                   min-x
                   (unchecked-inc y))
              max (morton/longs->morton-number
                   max-x
                   -1)]
          (recur i min max eid candidate))
        candidate))
    prev-candidate))

;;;; History

(defn- ->entity-tx [[k v]]
  (-> (c/decode-entity+vt+tt+tx-id-key-from k)
      (enrich-entity-tx v)))

(defn entity-history-seq-ascending
  ([i eid] ([i eid] (entity-history-seq-ascending i eid {})))
  ([i eid {{^Date start-vt :crux.db/valid-time, ^Date start-tt :crux.tx/tx-time} :start
           {^Date end-vt :crux.db/valid-time, ^Date end-tt :crux.tx/tx-time} :end
           :keys [with-corrections?]}]
   (let [seek-k (c/encode-entity+vt+tt+tx-id-key-to nil (c/->id-buffer eid) start-vt)]
     (-> (all-keys-in-prefix i (mem/limit-buffer seek-k (+ c/index-id-size c/id-size)) seek-k
                             {:reverse? true, :entries? true})
         (->> (map ->entity-tx))
         (cond->> end-vt (take-while (fn [^EntityTx entity-tx]
                                       (neg? (compare (.vt entity-tx) end-vt))))
                  start-tt (remove (fn [^EntityTx entity-tx]
                                     (neg? (compare (.tt entity-tx) start-tt))))
                  end-tt (filter (fn [^EntityTx entity-tx]
                                   (neg? (compare (.tt entity-tx) end-tt)))))
         (cond-> (not with-corrections?) (->> (partition-by :vt)
                                              (map last)))))))

(defn entity-history-seq-descending
  ([i eid] (entity-history-seq-descending i eid {}))
  ([i eid {{^Date start-vt :crux.db/valid-time, ^Date start-tt :crux.tx/tx-time} :start
           {^Date end-vt :crux.db/valid-time, ^Date end-tt :crux.tx/tx-time} :end
           :keys [with-corrections?]}]
   (let [seek-k (c/encode-entity+vt+tt+tx-id-key-to nil (c/->id-buffer eid) start-vt)]
     (-> (all-keys-in-prefix i (-> seek-k (mem/limit-buffer (+ c/index-id-size c/id-size))) seek-k
                             {:entries? true})
         (->> (map ->entity-tx))
         (cond->> end-vt (take-while (fn [^EntityTx entity-tx]
                                         (pos? (compare (.vt entity-tx) end-vt))))
                  start-tt (remove (fn [^EntityTx entity-tx]
                                    (pos? (compare (.tt entity-tx) start-tt))))
                  end-tt (filter (fn [^EntityTx entity-tx]
                                   (pos? (compare (.tt entity-tx) end-tt)))))
         (cond-> (not with-corrections?) (->> (partition-by :vt)
                                              (map first)))))))

;;;; IndexStore

(defrecord KvIndexStore [object-store snapshot]
  Closeable
  (close [_]
    (cio/try-close snapshot))

  kv/KvSnapshot
  (new-iterator ^java.io.Closeable [this]
    (kv/new-iterator snapshot))

  (get-value [this k]
    (kv/get-value snapshot k))

  db/IndexStore
  (av [this a min-v entity-resolver-fn]
    (let [attr-buffer (c/->id-buffer a)
          prefix (encode-ave-key-to nil attr-buffer)
          i (new-prefix-kv-iterator (kv/new-iterator this) prefix)]
      (some->> (encode-ave-key-to (.get idx/seek-buffer-tl)
                                  attr-buffer
                                  (buffer-or-value-buffer min-v))
               (kv/seek i)
               ((fn step [^DirectBuffer k]
                  (when k
                    (cons (MapEntry/create (.value (decode-ave-key->ev-from k))
                                           :crux.index.binary-placeholder/value)
                          (lazy-seq
                           (some->> (inc-unsigned-prefix-buffer k (- (.capacity k) c/id-size))
                                    (kv/seek i)
                                    (step))))))))))

  (ave [this a v min-e entity-resolver-fn]
    (let [attr-buffer (c/->id-buffer a)
          value-buffer (buffer-or-value-buffer v)
          prefix (encode-ave-key-to nil attr-buffer value-buffer)
          i (new-prefix-kv-iterator (kv/new-iterator this) prefix)]
      (some->> (encode-ave-key-to (.get idx/seek-buffer-tl)
                                  attr-buffer
                                  value-buffer
                                  (buffer-or-id-buffer min-e))
               (kv/seek i)
               ((fn step [^DirectBuffer k]
                  (when k
                    (let [eid (.eid (decode-ave-key->ev-from k))
                          eid-buffer (c/->id-buffer eid)]
                      (concat
                       (when-let [^EntityTx entity-tx (entity-resolver-fn eid-buffer)]
                         (let [version-k (encode-aecv-key-to (.get idx/seek-buffer-tl)
                                                             attr-buffer
                                                             eid-buffer
                                                             (c/->id-buffer (.content-hash entity-tx))
                                                             value-buffer)]
                           (when (kv/get-value this version-k)
                             [(MapEntry/create eid-buffer entity-tx)])))
                       (lazy-seq
                        (some->> (inc-unsigned-prefix-buffer k (.capacity k))
                                 (kv/seek i)
                                 (step)))))))))))

  (ae [this a min-e entity-resolver-fn]
    (let [attr-buffer (c/->id-buffer a)
          prefix (encode-aecv-key-to nil attr-buffer)
          i (new-prefix-kv-iterator (kv/new-iterator this) prefix)]
      (some->> (encode-aecv-key-to (.get idx/seek-buffer-tl)
                                     attr-buffer
                                     (buffer-or-id-buffer min-e))
               (kv/seek i)
               ((fn step [^DirectBuffer k]
                  (when k
                    (let [eid (.eid (decode-aecv-key->evc-from k))
                          eid-buffer (c/->id-buffer eid)]
                      (concat
                       (when (entity-resolver-fn eid-buffer)
                         [(MapEntry/create eid-buffer :crux.index.binary-placeholder/entity)])
                       (lazy-seq
                        (some->> (inc-unsigned-prefix-buffer k (- (.capacity k) c/id-size c/id-size))
                                 (kv/seek i)
                                 (step)))))))))))

  (aev [this a e min-v entity-resolver-fn]
    (let [attr-buffer (c/->id-buffer a)
          eid-buffer (buffer-or-id-buffer e)
          ^EntityTx entity-tx (entity-resolver-fn eid-buffer)
          content-hash-buffer (c/->id-buffer (.content-hash entity-tx))
          prefix (encode-aecv-key-to nil attr-buffer eid-buffer content-hash-buffer)
          i (new-prefix-kv-iterator (kv/new-iterator this) prefix)]
      (some->> (encode-aecv-key-to
                (.get idx/seek-buffer-tl)
                attr-buffer
                eid-buffer
                content-hash-buffer
                (buffer-or-value-buffer min-v))
               (kv/seek i)
               ((fn step [^DirectBuffer k]
                  (when k
                    (cons (MapEntry/create (.value (decode-aecv-key->evc-from k))
                                           entity-tx)
                          (lazy-seq (step (kv/next i))))))))))

  (entity-as-of [this eid valid-time transact-time]
    (with-open [i (kv/new-iterator snapshot)]
      (let [prefix-size (+ c/index-id-size c/id-size)
            eid-buffer (c/->id-buffer eid)
            seek-k (c/encode-entity+vt+tt+tx-id-key-to
                    (.get idx/seek-buffer-tl)
                    eid-buffer
                    valid-time
                    transact-time
                    nil)]
        (loop [k (kv/seek i seek-k)]
          (when (and k (mem/buffers=? seek-k k prefix-size))
            (let [entity-tx (safe-entity-tx (c/decode-entity+vt+tt+tx-id-key-from k))
                  v (kv/value i)]
              (if (<= (compare (.tt entity-tx) transact-time) 0)
                (when-not (mem/buffers=? c/nil-id-buffer v)
                  (enrich-entity-tx entity-tx v))
                (if morton/*use-space-filling-curve-index?*
                  (let [seek-z (c/encode-entity-tx-z-number valid-time transact-time)]
                    (when-let [[k v] (find-entity-tx-within-range-with-highest-valid-time i seek-z morton/z-max-mask eid-buffer nil)]
                      (when-not (= ::deleted-entity k)
                        v)))
                  (recur (kv/next i))))))))))

  (open-entity-history [this eid sort-order opts]
    (let [i (kv/new-iterator snapshot)
          entity-history-seq (case sort-order
                               :asc entity-history-seq-ascending
                               :desc entity-history-seq-descending)]
      (cio/->cursor #(.close i)
                    (entity-history-seq i eid opts))))

  (all-content-hashes [this eid]
    (with-open [i (kv/new-iterator snapshot)]
      (->> (all-keys-in-prefix i (encode-aecv-key-to (.get idx/seek-buffer-tl) (c/->id-buffer :crux.db/id) (c/->id-buffer eid)))
           (map decode-aecv-key->evc-from)
           (map #(.content-hash ^EntityValueContentHash %))
           (set))))

  (decode-value [this value-buffer eid-buffer]
    (assert (some? value-buffer))
    (assert (some? eid-buffer))
    (if (c/can-decode-value-buffer? value-buffer)
      (c/decode-value-buffer value-buffer)
      (some-> (kv/get-value this (encode-hash-cache-key-to (.get idx/seek-buffer-tl) eid-buffer value-buffer))
              (idx/<-nippy-buffer))))

  (encode-value [this value]
    (c/->value-buffer value))

  (open-nested-index-store [this]
    (->KvIndexStore object-store (lru/new-cached-snapshot snapshot false))))

;;;; Indexer

(defn content-idx-kvs [content-hash doc]
  (let [id (c/->id-buffer (:crux.db/id doc))
        content-hash (c/->id-buffer content-hash)]
    (->> (for [[k v] doc
               :let [k (c/->id-buffer k)]
               v (idx/vectorize-value v)
               :let [v-buf (c/->value-buffer v)]
               :when (pos? (.capacity v-buf))]
           (cond-> [(MapEntry/create (encode-ave-key-to nil k v-buf id) c/empty-buffer)
                    (MapEntry/create (encode-aecv-key-to nil k id content-hash v-buf) c/empty-buffer)]
             (not (c/can-decode-value-buffer? v-buf))
             (conj (MapEntry/create (encode-hash-cache-key-to nil id v-buf) (idx/->nippy-buffer v)))))
         (apply concat))))

(defrecord KvIndexer [kv-store object-store]
  db/Indexer
  (index-docs [this docs]
    (db/put-objects object-store (->> docs (into {} (filter (comp idx/evicted-doc? val)))))

    (with-open [snapshot (kv/new-snapshot kv-store)]
      (let [docs (->> docs
                      (into {} (remove (fn [[k doc]]
                                         (or (idx/keep-non-evicted-doc (db/get-single-object object-store snapshot (c/new-id k)))
                                             (idx/evicted-doc? doc)))))
                      not-empty)

            content-idx-kvs (when (seq docs)
                              (->> docs
                                   (mapcat (fn [[k doc]] (content-idx-kvs k doc)))))]

        (some->> (seq content-idx-kvs) (kv/store kv-store))
        (some->> (seq docs) (db/put-objects object-store))

        {:bytes-indexed (->> content-idx-kvs (transduce (comp (mapcat seq) (map mem/capacity)) +))
         :indexed-docs docs})))

  (unindex-docs [this docs]
    (with-open [snapshot (kv/new-snapshot kv-store)]
      (let [existing-docs (db/get-objects object-store snapshot (keys docs))]
        (->> existing-docs
             (mapcat (fn [[k doc]] (content-idx-kvs k doc)))
             keys
             (kv/delete kv-store))

        (db/put-objects object-store docs)

        {:unindexed-docs existing-docs})))

  (mark-tx-as-failed [this {:crux.tx/keys [tx-id] :as tx}]
    (kv/store kv-store [(idx/meta-kv ::latest-completed-tx tx)
                        [(c/encode-failed-tx-id-key-to nil tx-id) c/empty-buffer]]))

  (index-entity-txs [this tx entity-txs]
    (kv/store kv-store (->> (conj (mapcat etx->kvs entity-txs)
                                  (idx/meta-kv ::latest-completed-tx tx))
                            (into (sorted-map-by mem/buffer-comparator)))))

  (store-index-meta [_ k v]
    (idx/store-meta kv-store k v))

  (read-index-meta [_  k]
    (idx/read-meta kv-store k))

  (latest-completed-tx [this]
    (db/read-index-meta this ::latest-completed-tx))

  (tx-failed? [this tx-id]
    (with-open [snapshot (kv/new-snapshot kv-store)]
      (nil? (kv/get-value snapshot (c/encode-failed-tx-id-key-to nil tx-id)))))

  (open-index-store [this]
    (->KvIndexStore object-store (lru/new-cached-snapshot (kv/new-snapshot kv-store) true)))

  status/Status
  (status-map [this]
    {:crux.index/index-version (idx/current-index-version kv-store)
     :crux.doc-log/consumer-state (db/read-index-meta this :crux.doc-log/consumer-state)
     :crux.tx-log/consumer-state (db/read-index-meta this :crux.tx-log/consumer-state)}))

(def kv-indexer
  {:start-fn (fn [{:crux.node/keys [kv-store object-store]} args]
               (->KvIndexer kv-store object-store))
   :deps [:crux.node/kv-store :crux.node/object-store]})
