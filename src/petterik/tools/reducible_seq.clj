(ns petterik.tools.reducible-seq
  (:refer-clojure
    :exclude
    [map filter remove mapcat keep map-indexed keep-indexed partition-all take]))

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

(defprotocol IReducibleSeq
  (seq! [this])
  (reducible! [this]))

(deftype ReducibleSeq [^:unsynchronized-mutable ls
                       ^:unsynchronized-mutable xf_fn
                       ^:unsynchronized-mutable xf_args
                       ^:unsynchronized-mutable coll
                       met]
  IReducibleSeq
  (seq! [this]
    (locking this
      (condp = ls
        nil nil
        ::reduced (throw-already-reduced-ex)
        (let [lv (seq ls)]
          (when (= ::reduced xf_fn)
            (prn "WARN: Reducible seq was already reduced. Will create seq again. Please cache collection in a separate collection if you want to iterate over it more than once."))
          ;; Unset xf and coll for gc.
          (set! (.xf_fn this) nil)
          (set! (.xf_args this) nil)
          (set! (.coll this) nil)
          (set! (.ls this) lv)
          ;; TODO: chunked seq optimizations...?
          ;; By just using chunked seq optimizations we should
          ;; be able to just use that for seq.
          ;; making the even smaller.
          ;; This will basically become a wrapper around either
          ;; using sequence or eduction.
          ;; Where when using eduction twice will warn.
          lv))))

  (reducible! [this]
    (locking this
      (if (some? xf_fn)
        (let [edu (eduction (apply xf_fn xf_args) coll)]
          ;; Unset xf and coll to avoid coming to this branch again.
          (set! (.xf_fn this) ::reduced)
          (set! (.xf_args this) nil)
          (set! (.coll this) nil)
          edu)
        (seq! this))))

  java.io.Serializable
  clojure.lang.IObj
  (meta [this] met)
  (withMeta [this m]
    (if (identical? met m)
      this
      (ReducibleSeq. ls xf_fn xf_args coll m)))

  clojure.lang.Seqable
  (seq [this]
    (seq! this))

  clojure.lang.IHashEq
  (hasheq [this]
    (clojure.lang.Murmur3/hashOrdered (seq! this)))

  clojure.lang.Sequential

  clojure.lang.IReduce
  (reduce [this rf]
    (clojure.core/reduce rf (seq! this)))

  clojure.lang.IReduceInit
  (reduce [this rf init]
    (clojure.core/reduce rf init (reducible! this))))

(defmethod print-method ReducibleSeq [c w]
  (prn (seq c))
  (print-simple (seq c) w))

(defn reducible-seq
  ([s coll xf-fn & xf-args]
   (ReducibleSeq. s xf-fn xf-args coll nil)))

(defmacro defreducible [clj-fn]
  (let [arg1 (gensym)
        arg2 (gensym)
        sym (symbol clj-fn)
        fq-sym (symbol "clojure.core" clj-fn)]
    `(defn ~sym
       ([~arg1] (~fq-sym ~arg1))
       ([~arg1 ~arg2]
        (reducible-seq
          (~fq-sym ~arg1 ~arg2)
          ~arg2
          ~fq-sym
          ~arg1)))))


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
