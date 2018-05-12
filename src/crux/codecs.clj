(ns crux.codecs
  (:require [crux.byte-utils :refer [md5 to-byte-array]])
  (:import [java.nio ByteBuffer]
           [java.util Date]))

(def ^{:tag 'long}
  max-timestamp (.getTime #inst "9999-12-30"))

(defn- encode-bytes [^ByteBuffer bb #^bytes bs]
  (.put bb bs))

(defn encode-string [^ByteBuffer bb ^String s]
  (.put bb #^bytes (.getBytes s)))

(defn decode-string [^ByteBuffer bb]
  (let [ba (byte-array (.remaining bb))]
    (.get bb ba)
    (String. ba)))

(defn- keyword->string [k]
  (-> k str (subs 1)))

(def binary-types {:int32 [4 #(.putInt ^ByteBuffer %1 %2) #(.getInt ^ByteBuffer %)]
                   :int64 [8 #(.putLong ^ByteBuffer %1 %2) #(.getLong ^ByteBuffer %)]
                   :id [4 #(.putInt ^ByteBuffer %1 %2) #(.getInt ^ByteBuffer %)]
                   :reverse-ts [8
                                (fn [^ByteBuffer b ^Date x] (.putLong b (- max-timestamp (.getTime x))))
                                #(Date. (- max-timestamp (.getLong ^ByteBuffer %)))]
                   :string [(fn [^String s] (alength (.getBytes s))) encode-string decode-string]
                   :keyword [(fn [k] (alength (.getBytes ^String (keyword->string k))))
                             (fn [^ByteBuffer bb k] (encode-string bb (keyword->string k)))
                             #(-> % decode-string keyword)]
                   :md5 [16
                         (fn [b x] (encode-bytes b (-> x to-byte-array md5)))
                         (fn [^ByteBuffer b] (.get b (byte-array 16)))]})

(defprotocol Codec
  (encode [this v])
  (decode [this v]))

(defn- resolve-data-type [t]
  (or (and (vector? t) t)
      (get binary-types t)))

(defn compile-frame [& args]
  (let [pairs (->> args
                   (partition 2)
                   (map (fn [[k t]]
                          (let [datatype (resolve-data-type t)]
                            (when-not datatype
                              (throw (IllegalArgumentException. (str "Unknown datatype: " t))))
                            [k datatype]))))
        fixed-length (reduce + (for [[k [length]] pairs :when (number? length)]
                                 length))
        length-fn (first (for [[k [length-f]] pairs :when (fn? length-f)]
                           (fn [m] (length-f (get m k)))))]
    (reify Codec
      (encode [this m]
        (let [length (if length-fn (+ fixed-length (length-fn m)) fixed-length)
              b (ByteBuffer/allocate length)]
          (doseq [[k [_ f]] pairs
                  :let [v (get m k)]]
            (f b v))
          b))
      (decode [_ v]
        (let [b (ByteBuffer/wrap #^bytes v)
              m (transient {})]
          (doseq [[k [_ _ f]] pairs]
            (assoc! m k  (f b)))
          (persistent! m))))))

(defn compile-header-frame [[header-k header-frame] frames]
  (reify Codec
    (encode [_ v]
      (let [frame (get frames (get v header-k))]
        (encode frame v)))
    (decode [_ v]
      ;; Todo could perhaps eliminate the double read of the prefix
      (let [b (ByteBuffer/wrap v)
            [_ _ f] (if (vector? header-frame)
                      header-frame
                      (get binary-types header-frame))
            codec (get frames (f b))]
        (decode codec v)))))

(defmacro compile-enum [& vals]
  `[1
    (fn [^ByteBuffer bb# ~'k]
      (let [v# (case ~'k
                 ~@(flatten (for [[i k] (map-indexed vector vals)]
                              [k (byte i)])))]
        (assert v#)
        (.put bb# ^Byte v#)))
    (fn [^ByteBuffer bb#]
      (let [k# (case (.get bb#)
                 ~@(flatten (for [[i k] (map-indexed vector vals)]
                              [(byte i) k])))]
        (assert k#)
        k#))])
