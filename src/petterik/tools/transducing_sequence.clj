(ns petterik.tools.transducing-sequence)

(set! *warn-on-reflection* true)

(def preserving-reduced @#'clojure.core/preserving-reduced)

;; TODO: Inline buffer:<x> in transducing-sequence.

(defn- buffer:conj!
  ([] (java.util.ArrayList.))
  ([buf] buf)
  ([^java.util.ArrayList buf x]
   (.add buf x)
   buf))

(def ^:const ^long xf-seq-buffer-capacity
  "Initial size of xf-seq's internal buffer.

  Chooses the capacity less than 11 which takes
  the least amount of times to grow an ArrayList
  past 32 (the size of chunked-seqs) which also
  minimizes the extra space"
  (->> (for [capacity (range 1 11)
             :let [growth (transduce (halt-when #(<= 32 %) conj)
                            conj
                            []
                            (iterate #(* % 3) capacity))
                   extra-space (- (last growth) 32)]]
         [(count growth) extra-space capacity])
    (sort)
    (first)
    (last)
    (long)))
;; => 4

(comment
  ;; Regarding ArrayList capacity

  ;; => 4

  ;; Number 11 is the best number within the range of
  ;; 1 and 32, but it's quite large for non-chunked seqs.
  )

(def xf-seq
  (letfn [(buffer-cons [^java.util.ArrayList buf more]
            ;; TODO: As `case` was a massive improvement
            ;;       (2x faster on a dechunked example)
            ;;       Add additional cases? for 2, 3, 4, etc?
            (case (.size buf)
              0 more
              1 (cons (.get buf 0) more)
              ;; else
              (clojure.lang.ChunkedCons.
                (clojure.lang.ArrayChunk. (.toArray buf))
                more)))

          (yield-aseq [s xf rrf buf]
            (buffer-cons buf
              (lazy-seq
                (step s xf rrf buf))))

          (step* [s xf rrf buf]
            (if (chunked-seq? s)
              (let [buf (unreduced (.reduce (chunk-first s) rrf buf))]
                (if (reduced? buf)
                  (buffer-cons (xf (deref buf)) nil)
                  (yield-aseq (chunk-rest s) xf rrf buf)))
              (let [buf (xf buf (first s))
                    s (rest s)]
                (if (reduced? buf)
                  (buffer-cons (xf (deref buf)) nil)
                  (if (clojure.lang.Numbers/isPos (.size ^java.util.ArrayList buf))
                    (yield-aseq s xf rrf buf)
                    ;; Unable to get an item, recur inside this
                    ;; function with the next seq to avoid blowing
                    ;; the stack.
                    (if-some [s (seq s)]
                      (recur s xf rrf buf)
                      (buffer-cons (xf buf) nil)))))))

          ;; Gets the next chunk of elements from either
          ;; a chunked seq of a non-chunked seq.
          (step [s xf rrf buf]
            (.clear ^java.util.ArrayList buf)
            (if-some [s (seq s)]
              (step* s xf rrf buf)
              (buffer-cons (xf buf) nil)))]
    (fn [xform coll]
      (lazy-seq
        (when-some [s (seq coll)]
          (let [xf (xform buffer:conj!)
                rrf (preserving-reduced xf)]
            (step s xf rrf (java.util.ArrayList. xf-seq-buffer-capacity))))))))
