(ns petterik.tools.xf-seq-core)

(def ^:static xf-seq
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
  (let [step (clojure.lang.Var/create)

        end-cons (fn [^java.util.ArrayList buf]
                   (let [size (.size buf)]
                     (case* size 0 0
                       ;; else
                       (clojure.lang.ChunkedCons. (clojure.lang.ArrayChunk. (.toArray buf)) nil)
                       ;; cases
                       {0 [0 nil]
                        1 [1 (cons (.get buf 0) nil)]}
                       :compact
                       :int)))

        step* (fn [s xf ^java.util.ArrayList buf]
                (let [chunked? (chunked-seq? s)
                      ret (if chunked?
                                (.reduce (chunk-first s) xf buf)
                                (xf buf (first s)))]
                  (if (clojure.lang.RT/isReduced ret)
                    (end-cons (xf (.deref ^clojure.lang.Reduced ret)))
                    (let [s (if chunked? (chunk-rest s) (rest s))
                          size (.size buf)]
                      (case* size 0 0
                        ;; else
                        (clojure.lang.ChunkedCons.
                          (clojure.lang.ArrayChunk. (.toArray buf))
                          (lazy-seq (step s xf buf)))
                        ;; cases
                        {0 [0 (let [s (seq s)]
                                (if s (recur s xf buf) (end-cons (xf buf))))]
                         1 [1 (cons (.get buf 0) (lazy-seq (step s xf buf)))]}
                        :compact
                        :int)))))

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
                          (end-cons (xf buf))))))
    (let [step (.deref step)]
      (fn [xform coll]
        (lazy-seq
          (let [s (seq coll)]
            (when s
              (step s (xform arr-conj!) (java.util.ArrayList. 4)))))))))