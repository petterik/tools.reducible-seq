(ns petterik.tools.xf-seq-core)

(def ^:static xf-seq-1
  (let [step (clojure.lang.Var/create)

        buffer-cons (fn [^java.util.ArrayList buf more]
                      (let [s (.size buf)]
                        (if (== s 1)
                          (cons (.get buf 0) more)
                          (if (== s 0)
                            more
                            (clojure.lang.ChunkedCons.
                              (clojure.lang.ArrayChunk. (.toArray buf))
                              more)))))

        yield-aseq (fn [s xf buf]
                     (buffer-cons buf
                       (lazy-seq
                         (step s xf buf))))

        step* (fn [s xf buf]
                (if (chunked-seq? s)
                  (let [buf (.reduce (chunk-first s) xf buf)]
                    (if (clojure.lang.RT/isReduced buf)
                      (buffer-cons (xf (.deref ^clojure.lang.Reduced buf)) nil)
                      (yield-aseq (chunk-rest s) xf buf)))
                  (let [buf (xf buf (first s))
                        s (rest s)]
                    (if (clojure.lang.RT/isReduced buf)
                      (buffer-cons (xf (.deref ^clojure.lang.Reduced buf)) nil)
                      (if (clojure.lang.Numbers/isPos (.size ^java.util.ArrayList buf))
                        (yield-aseq s xf buf)
                        ;; Unable to get an item, recur inside this
                        ;; function with the next seq to avoid blowing
                        ;; the stack.
                        (let [s (seq s)]
                          (if s
                            (recur s xf buf)
                            (buffer-cons (xf buf) nil))))))))

        arr-conj! (fn
                    ([] (java.util.ArrayList.))
                    ([buf] buf)
                    ([^java.util.ArrayList buf x]
                     (.add buf x)
                     buf))]
    (.bindRoot step (fn [s xf buf]
                      (.clear ^java.util.ArrayList buf)
                      (let [s (seq s)]
                        (if s
                          (step* s xf buf)
                          (buffer-cons (xf buf) nil)))))
    (let [step @step]
      (fn [xform coll]
        (lazy-seq
          (let [s (seq coll)]
            (when s
              (step s (xform arr-conj!) (java.util.ArrayList. 4)))))))))

(def ^:static xf-seq-2
  (let [arr-conj! (fn
                    ([] (java.util.ArrayList.))
                    ([buf] buf)
                    ([^java.util.ArrayList buf x]
                     (.add buf x)
                     buf))
        step (fn self [s xf ^java.util.ArrayList buf]
               (if s
                 (let [chunked? (chunked-seq? s)
                       ret (if chunked?
                             (.reduce (chunk-first s) xf buf)
                             (xf buf (first s)))]
                   (if (clojure.lang.RT/isReduced ret)
                     ;; TODO: is the reduced ret always buf?
                     (recur nil xf (.deref ^clojure.lang.Reduced ret))
                     (let [s (if chunked? (chunk-rest s) (rest s))
                           size (.size buf)]
                       (case* size 0 0
                         ;; else
                         (clojure.lang.ChunkedCons. (clojure.lang.ArrayChunk. (.toArray buf))
                           (lazy-seq
                             (.clear buf)
                             (self (seq s) xf buf)))
                         ;; cases
                         {0 [0 (recur (seq s) xf buf)]
                          1 [1 (cons (.get buf 0)
                                 (lazy-seq
                                   (.clear buf)
                                   (self (seq s) xf buf)))]}
                         :compact
                         :int))))
                 (let [^java.util.ArrayList buf (xf buf)
                       size (.size buf)]
                   (case* size 0 0
                     ;; else
                     (clojure.lang.ChunkedCons. (clojure.lang.ArrayChunk. (.toArray buf)) nil)
                     ;; cases
                     {0 [0 nil]
                      1 [1 (cons (.get buf 0) nil)]}
                     :compact
                     :int))))]
    (fn [xform coll]
      (lazy-seq
        (let [s (seq coll)]
          (when s
            (step s (xform arr-conj!) (java.util.ArrayList. 4))))))))

;; Final form?

(defn ^:private ^:static xf-seq-step
  [^clojure.lang.ISeq s ^clojure.lang.IFn xf ^java.util.ArrayList buf]
  (if s
    (if (chunked-seq? s)
      (if (clojure.lang.Util/identical buf (.reduce (chunk-first ^clojure.lang.IChunkedSeq s) xf buf))
        (let [size (.size buf)]
          (case* size 0 0
            ;;else
            (clojure.lang.ChunkedCons. (clojure.lang.ArrayChunk. (.toArray buf))
              ^clojure.lang.ISeq
              (do
                (.clear buf)
                (lazy-seq
                  (xf-seq-step (chunk-next ^clojure.lang.IChunkedSeq s) xf buf))))
            {0 [0 (recur (chunk-next ^clojure.lang.IChunkedSeq s) xf buf)]
             1 [1 (clojure.lang.Cons. (.get buf 0)
                    ^clojure.lang.ISeq
                    (do
                      (.clear buf)
                      (lazy-seq
                        (xf-seq-step (chunk-next ^clojure.lang.IChunkedSeq s) xf buf))))]}
            :compact
            :int))
        (recur nil xf buf))
      (if (clojure.lang.Util/identical buf (xf buf (.first s)))
        (let [size (.size buf)]
          (case* size 0 0
            (clojure.lang.ChunkedCons. (clojure.lang.ArrayChunk. (.toArray buf))
              ^clojure.lang.ISeq
              (do
                (.clear buf)
                (lazy-seq
                  (xf-seq-step (.next s) xf buf))))
            {0 [0 (recur (.next s) xf buf)]
             1 [1 (clojure.lang.Cons.
                    (.get buf 0)
                    ^clojure.lang.ISeq
                    (do
                      (.clear buf)
                      (lazy-seq
                        (xf-seq-step (.next s) xf buf))))]}
            :compact
            :int))
        (recur nil xf buf)))
    (do
      (xf buf)
      (let [size (.size buf)]
        (case* size 0 0
          ;; else
          (clojure.lang.ChunkedCons. (clojure.lang.ArrayChunk. (.toArray buf)) nil)
          ;; cases
          {0 [0 nil]
           1 [1 (clojure.lang.Cons. (.get buf 0) nil)]}
          :compact
          :int)))))

(def ^:static xf-seq-arr-conj!
  (fn
    ([] (java.util.ArrayList.))
    ([buf] buf)
    ([buf x]
     (.add ^java.util.ArrayList buf x)
     buf)))

(def ^:static xf-seq
  (fn xf-seq [xform coll]
    (lazy-seq
      (let [s (seq coll)]
        (if s
          (xf-seq-step s (xform xf-seq-arr-conj!) (java.util.ArrayList. 4)))))))
