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
            ;; Sets arraylist capacity to 4 such that it's small
            ;; which is good for non-chunked seqs and grows larger
            ;; than 32 (size of standard chunk-seq) after 2 growths:
            ;; Size after 1st growth: 4+8=12
            ;; Size after 2nd growth: 12+24=36
            ;; TODO: Benchmark whether this is a good number or not.
            (step s xf rrf (java.util.ArrayList. 4))))))))

(defn- buffer:nth [^java.util.ArrayList buf ^long index]
  (.get buf index))

(defn- buffer:size ^long [^java.util.ArrayList buf]
  (.size buf))

(defn buffer:clear! [^java.util.ArrayList buf]
  (.clear buf)
  buf)

(defn buffer:persistent! [^java.util.ArrayList buf]
  (clojure.lang.PersistentVector/create ^java.lang.Iterable buf))

(defn transducing-sequence [xform coll]
  (let [xf (xform buffer:conj!)
        rrf (preserving-reduced xf)
        buf (java.util.ArrayList.)]
    (letfn [(return-step [s chunk]
              ;; TODO: As this case was a massive improvement
              ;;       (2x faster on the dechunked example)
              ;;       Add additional cases? for 2, 3, 4, ...?
              (case (buffer:size chunk)
                0 (step s)
                1 (lazy-seq
                    (cons (buffer:nth chunk 0)
                      (step s)))
                ;; else
                (concat
                  (buffer:persistent! chunk)
                  (lazy-seq
                    (step s)))))

            ;; Returning the next elements by applying one item
            ;; from the seq at a time to the transducing function.
            (next-elem [s]
              (loop [buf (xf (buffer:clear! buf) (first s)) s (next s)]
                (cond
                  ;; Halting transduction.
                  (reduced? buf)
                  (seq (buffer:persistent! (xf (deref chunk))))

                  ;; The buffer has at least one item, return.
                  (clojure.lang.Numbers/isPos (buffer:size buf))
                  (return-step s buf)

                  (nil? s)
                  (seq (buffer:persistent! (xf (buffer:clear! buf))))

                  :else
                  (recur (xf buf (first s)) (next s)))))

            (next-chunk [s]
              (let [cf (chunk-first s)
                    chunk (unreduced (.reduce cf rrf (buffer:clear! buf)))]
                (if (reduced? chunk)
                  (seq (buffer:persistent! (xf (deref chunk))))
                  (return-step (chunk-next s) chunk))))

            ;; Gets the next chunk of elements from either
            ;; a chunked seq of a non-chunked seq.
            (step [s]
              (when-some [s (seq s)]
                (if (chunked-seq? s)
                  (next-chunk s)
                  (next-elem s))))]
      (lazy-seq
        (step coll)))))
