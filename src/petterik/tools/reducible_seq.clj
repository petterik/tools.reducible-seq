(ns petterik.tools.reducible-seq
  (:require
    [petterik.tools.transducing-sequence :as xf-seq]))

(def ^:dynamic *warn-on-use-after-reduced*
  "Will throw on `false`."
  true)

(comment
  (mapv symbol to-overwrite))

(defn throw-already-reduced-ex []
  (throw
    (ex-info
      (str "Cannot use the reducible seq after it's been reduced."
        " Store the collection in a separate coll before using it twice.")
      {})))

;; Only prints the warning once.
(def print-reuse-warning
  (memoize
    (fn [type]
      (prn (format "WARN: Reducible seq was already reduced. Will create %s again. Please cache collection in a separate collection if you want to iterate over it more than once."
             type)))))

(defprotocol IReducibleSeq
  (seq! ^clojure.lang.LazySeq [this] "Return a possibly cached lazy seq")
  (reducible! [this] "Return a reducible object."))

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
    (seq (seq! this)))

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
    (clojure.core/reduce rf init (reducible! this)))

  ;; Backwards compatibility with LazySeq
  java.util.List
  (toArray [this]
    (.toArray (seq! this)))
  (toArray [this a]
    (.toArray (seq! this) a))
  (containsAll [this coll]
    (.containsAll (seq! this) coll))
  (contains [this obj]
    (.contains (seq! this) obj))
  (size [this]
    (.size (seq! this)))
  (isEmpty [this]
    (.isEmpty (seq! this)))
  (iterator [this]
    (.iterator (seq! this)))
  (subList [this fromIdx toIdx]
    (.subList (seq! this) fromIdx toIdx))
  (indexOf [this obj]
    (.indexOf (seq! this) obj))
  (lastIndexOf [this obj]
    (.lastIndexOf (seq! this) obj))
  (listIterator [this]
    (.listIterator (seq! this)))
  (listIterator [this idx]
    (.listIterator (seq! this) idx))
  (get [this idx]
    (.get (seq! this) idx))
  (add [this obj]
    (.add (seq! this) obj))
  (add [this idx obj]
    (.add (seq! this) idx obj))
  (addAll [this idx coll]
    (.addAll (seq! this) idx coll))
  (addAll [this coll]
    (.addAll (seq! this) coll))
  (clear [this]
    (.clear (seq! this)))
  (retainAll [this coll]
    (.retainAll (seq! this) coll))
  (removeAll [this coll]
    (.removeAll (seq! this) coll))
  (set [this idx obj]
    (.set (seq! this) idx obj))
  #_(remove [this obj] (unsupported-ex))
  #_(remove [this idx] (unsupported-ex))
  )

(defmethod print-method ReducibleSeq [c w]
  (print-simple (seq c) w))

(defn reducible-seq
  ([xf coll]
   (ReducibleSeq. nil xf coll nil)))

(comment

  (defmacro defreducible [clj-fn]
    (let [arg1 (gensym)
          arg2 (gensym)
          sym (symbol clj-fn)
          fq-sym (symbol "clojure.core" clj-fn)]
      `(defn ~sym
         ([~arg1] (~fq-sym ~arg1))
         ([~arg1 ~arg2]
          (reducible-seq (~fq-sym ~arg1) ~arg2)))))


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

  (do
    `(do
       ~@(for [f# to-overwrite]
           `(~'defreducible ~f#))))

  (do
    (defreducible "map")
    (defreducible "filter")
    (defreducible "remove")
    (defreducible "mapcat")
    (defreducible "keep")
    (defreducible "keep-indexed")
    (defreducible "map-indexed")
    (defreducible "partition-all")
    (defreducible "take")))
