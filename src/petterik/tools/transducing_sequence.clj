(ns petterik.tools.transducing-sequence)

(set! *warn-on-reflection* true)

(def preserving-reduced @#'clojure.core/preserving-reduced)

;; TODO: Inline buffer:<x> in transducing-sequence.
(defn- buffer:size ^long [^java.util.ArrayList buf]
  (.size buf))

(defn- buffer:conj!
  ([] (java.util.ArrayList.))
  ([buf] buf)
  ([^java.util.ArrayList buf x]
   (.add buf x)
   buf))

(defn- buffer:nth [^java.util.ArrayList buf ^long index]
  (.get buf index))

(defn buffer:persistent! [^java.util.ArrayList buf]
  (clojure.lang.PersistentVector/create ^java.lang.Iterable buf))

(defn buffer:clear! [^java.util.ArrayList buf]
  (.clear buf)
  buf)

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
