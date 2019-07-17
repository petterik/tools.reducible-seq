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

(defn- seq!
  ^clojure.lang.LazySeq
  [state]
  (locking state
    (let [{:keys [ls] :as m} @state]
      (condp = ls
        nil
        (let [ls (xf-seq/transducing-sequence (:xf m) (:coll m))]
          (vreset! state {:ls ls})
          ls)

        ;; Warn on reduced and re-create the sequence.
        ::reduced
        (if *warn-on-use-after-reduced*
          (do
            (print-reuse-warning "seq")
            (vswap! state dissoc :ls)
            (seq! state))
          (throw-already-reduced-ex))

        ;; else, return the created sequence.
        ls))))

(defn- reducible!
  ^clojure.lang.IReduceInit
  [state]
  (locking state
    (let [{:keys [ls xf] :as m} @state]
      (cond
        (seq? ls)
        ls

        (some? xf)
        (let [edu (eduction xf (:coll m))]
          (if (= ::reduced ls)
            (print-reuse-warning "reducible")
            (vswap! state :ls ::reduced))

          (when (not *warn-on-use-after-reduced*)
            (vswap! state dissoc :xf :coll))
          edu)

        :else
        (throw-already-reduced-ex)))))

(defn- reducible-seq* [state]
  (reify
    clojure.lang.Seqable
    (seq [_]
      (seq (seq! state)))

    clojure.lang.IHashEq
    (hasheq [_]
      (clojure.lang.Murmur3/hashOrdered (seq! state)))

    java.lang.Object
    (hashCode [this]
      (if-some [s (.seq this)]
        (clojure.lang.Util/hash s)
        1))
    (equals [this obj]
      (if-some [s (.seq this)]
        (.equals s obj)
        (nil? (clojure.lang.RT/seq obj))))


    clojure.lang.Sequential

    clojure.lang.IPersistentCollection
    (count [_]
      (clojure.lang.RT/length (seq! state)))
    (cons [_ obj]
      (clojure.lang.RT/cons obj (seq! state)))
    (empty [_]
      (clojure.lang.PersistentList/EMPTY))
    (equiv [this obj]
      (if-some [s (.seq this)]
        (.equiv s obj)
        (nil? (clojure.lang.RT/seq obj))))

    clojure.lang.IReduce
    (reduce [_ rf]
      (clojure.core/reduce rf (seq! state)))

    clojure.lang.IReduceInit
    (reduce [_ rf init]
      (clojure.core/reduce rf init (reducible! state)))

    clojure.lang.IPending

    ;; Backwards compatibility with LazySeq
    java.io.Serializable
    java.util.List
    (toArray [_]
      (.toArray (seq! state)))
    (toArray [_ a]
      (.toArray (seq! state) a))
    (containsAll [_ coll]
      (.containsAll (seq! state) coll))
    (contains [_ obj]
      (.contains (seq! state) obj))
    (size [_]
      (.size (seq! state)))
    (isEmpty [_]
      (.isEmpty (seq! state)))
    (iterator [_]
      (.iterator (seq! state)))
    (subList [_ fromIdx toIdx]
      (.subList (seq! state) fromIdx toIdx))
    (indexOf [_ obj]
      (.indexOf (seq! state) obj))
    (lastIndexOf [_ obj]
      (.lastIndexOf (seq! state) obj))
    (listIterator [_]
      (.listIterator (seq! state)))
    (listIterator [_ idx]
      (.listIterator (seq! state) idx))
    (get [_ idx]
      (.get (seq! state) idx))
    (add [_ obj]
      (.add (seq! state) obj))
    (add [_ idx obj]
      (.add (seq! state) idx obj))
    (addAll [_ idx coll]
      (.addAll (seq! state) idx coll))
    (addAll [_ coll]
      (.addAll (seq! state) coll))
    (clear [_]
      (.clear (seq! state)))
    (retainAll [_ coll]
      (.retainAll (seq! state) coll))
    (removeAll [_ coll]
      (.removeAll (seq! state) coll))
    (set [_ idx obj]
      (.set (seq! state) idx obj))
    #_(remove [_ obj] (unsupported-ex))
    #_(remove [_ idx] (unsupported-ex))
    ))

(defn reducible-seq [xf coll]
  ;; Calling the r-lazy-seq* with the initial state
  ;; to make sure we're not closing over xf and coll
  ;; avoiding a potential memory leak.
  (reducible-seq*
    (volatile! {:xf xf :coll coll})))

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


  (def to-special-case {"map" (fn [fq-sym arg1 arg2]
                                (let [more-sym (gensym)]
                                  `[([~arg1 ~arg2 ~'& ~more-sym]
                                     (map #(apply ~arg1 %)
                                       ((fn ~'step [cs#]
                                          (lazy-seq
                                            (let [ss# (map seq cs#)]
                                              (when (every? identity ss#)
                                                (cons (map first ss#) (~'step (map rest ss#)))))))
                                        (cons ~arg2 ~more-sym))))]))
                        "mapcat" nil})
  (def to-overwrite
    ["filter"
     "remove"
     "keep"
     "keep-indexed"
     "map-indexed"
     "partition-all"
     "take"])

  (do
    `(do
       ~@(for [f# to-overwrite]
           `(~'defreducible ~f# nil))))

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


  (defmacro defreducible2
    [clj-fn variadic-fn]
    (let [arg1 (gensym)
          arg2 (gensym)
          fq-sym (symbol "clojure.core" clj-fn)]
      `(alter-var-root (var ~fq-sym)
         (fn [~'_]
           (fn
             ([~arg1] (~fq-sym ~arg1))
             ([~arg1 ~arg2]
              (reducible-seq (~fq-sym ~arg1) ~arg2))
             ~@(when-some [f variadic-fn]
                 (f fq-sym arg1 arg2)))))))

  (macroexpand-1 '(defreducible2 "take" nil))
  (defreducible2 "take" nil)
  (macroexpand-1 '(defreducible2 "map" (get to-special-case "map")))

  ((get to-special-case "map") 'clojure.core/map 'f 'coll)

  (do `(do
      ~@(range 10)))

  )
