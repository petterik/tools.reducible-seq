(ns petterik.tools.reducible-seq
  (:require
    [petterik.tools.transducing-sequence :as xf-seq])
  (:refer-clojure
    :exclude
    [map filter remove mapcat keep map-indexed keep-indexed partition-all take]))

(def ^:dynamic *warn-on-use-after-reduced* true)

(def to-overwrite
  ["map"
   "filter"
   "remove"
   "mapcat"
   "keep"
   "keep-indexed"
   "map-indexed"
   "partition-all"
   "take"])

(comment
  (mapv symbol to-overwrite))

(defn throw-already-reduced-ex []
  (throw
    (ex-info
      (str "Cannot use the reducible seq after it's been reduced."
        " Store the collection in a separate coll before using it twice.")
      {})))

(defn print-reuse-warning [type]
  (prn (format "WARN: Reducible seq was already reduced. Will create %s again. Please cache collection in a separate collection if you want to iterate over it more than once."
         type)))

(defprotocol IReducibleSeq
  (seq! [this])
  (reducible! [this]))

(deftype ReducibleSeq [^:unsynchronized-mutable ls
                       ^:unsynchronized-mutable xf
                       ^:unsynchronized-mutable coll
                       met]
  IReducibleSeq
  (seq! [this]
    (locking this
      (condp = ls
        nil
        (let [ls (xf-seq/transducing-sequence xf coll)]
          (set! (.ls this) ls)
          (set! (.xf this) nil)
          (set! (.coll this) nil)
          ls)

        ;; Warn on reduced and re-create the sequence.
        ::reduced
        (if *warn-on-use-after-reduced*
          (do
            (print-reuse-warning "seq")
            (set! (.ls this) nil)
            (seq! this))
          (throw-already-reduced-ex))

        ;; else, return the created sequence.
        ls)))

  (reducible! [this]
    (locking this
      (cond
        (seq? ls)
        ls

        (some? xf)
        (let [edu (eduction xf coll)]
          (if (= ::reduced ls)
            (print-reuse-warning "reducible")
            (set! (.ls this) ::reduced))
          ;; When we're just warning about reuse, we need the
          ;; xf and coll to create the seq.
          (when (not *warn-on-use-after-reduced*)
            (set! (.xf this) nil)
            (set! (.coll this) nil))
          edu)

        :else
        (throw-already-reduced-ex))))

  java.io.Serializable
  clojure.lang.IObj
  (meta [this] met)
  (withMeta [this m]
    (if (identical? met m)
      this
      (ReducibleSeq. ls xf coll m)))

  clojure.lang.Seqable
  (seq [this]
    (seq! this))

  clojure.lang.IHashEq
  (hasheq [this]
    (clojure.lang.Murmur3/hashOrdered (seq! this)))

  java.lang.Object
  (hashCode [this]
    (if-some [s (seq this)]
      (clojure.lang.Util/hash s)
      1))
  (equals [this obj]
    (if-some [s (seq this)]
      (.equals s obj)
      (nil? (clojure.lang.RT/seq obj))))

  clojure.lang.Sequential

  clojure.lang.IPersistentCollection
  (count [this]
    (clojure.lang.RT/length (seq! this)))
  (cons [this obj]
    (clojure.lang.RT/cons obj (seq! this)))
  (empty [this]
    (clojure.lang.PersistentList/EMPTY))
  (equiv [this obj]
    (if-some [s (seq! this)]
      (.equiv s obj)
      (nil? (clojure.lang.RT/seq obj))))

  clojure.lang.IReduce
  (reduce [this rf]
    (clojure.core/reduce rf (seq! this)))

  clojure.lang.IReduceInit
  (reduce [this rf init]
    (clojure.core/reduce rf init (reducible! this))))

(defmethod print-method ReducibleSeq [c w]
  (print-simple (seq c) w))

(defn reducible-seq
  ([xf coll]
   (ReducibleSeq. nil xf coll nil)))

(defmacro defreducible [clj-fn]
  (let [arg1 (gensym)
        arg2 (gensym)
        sym (symbol clj-fn)
        fq-sym (symbol "clojure.core" clj-fn)]
    `(defn ~sym
       ([~arg1] (~fq-sym ~arg1))
       ([~arg1 ~arg2]
        (reducible-seq (~fq-sym ~arg1) ~arg2)))))

(comment
  (do
    `(do
       ~@(for [f# to-overwrite]
           `(~'defreducible ~f#)))))

(do
  (defreducible "map")
  (defreducible "filter")
  (defreducible "remove")
  (defreducible "mapcat")
  (defreducible "keep")
  (defreducible "keep-indexed")
  (defreducible "map-indexed")
  (defreducible "partition-all")
  (defreducible "take"))
