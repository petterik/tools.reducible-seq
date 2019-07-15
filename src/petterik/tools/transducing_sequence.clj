(ns petterik.tools.transducing-sequence)

(set! *warn-on-reflection* true)

(def preserving-reduced @#'clojure.core/preserving-reduced)

(defprotocol ITransientBuffer
  (clear [this]))

(deftype TransientBuffer [buf]
  ITransientBuffer
  (clear [this]
    (.clear ^java.util.ArrayList buf)
    this)

  clojure.lang.Indexed
  (nth [this idx]
    (.get ^java.util.ArrayList buf idx))

  clojure.lang.ITransientCollection
  (conj [this x]
    (.add ^java.util.ArrayList buf x)
    this)
  (persistent [this]
    (clojure.lang.PersistentVector/create ^java.util.ArrayList buf)))

(defn buffer-size ^long [^TransientBuffer buf]
  (.size ^java.util.ArrayList (.buf buf)))

(defn transducing-sequence [xf coll]
  (let [xf (xf conj!)
        rrf (preserving-reduced xf)
        buf (TransientBuffer. (java.util.ArrayList.))]
    (letfn [(return-step [s chunk]
              ;; TODO: As this case was a massive improvement
              ;;       (2x faster on the dechunked example)
              ;;       Add additional cases? for 2, 3, 4, ...?
              (case (buffer-size chunk)
                0 (step s)
                1 (lazy-seq
                    (cons (nth chunk 0)
                      (step s)))
                ;; else
                (lazy-cat
                  (persistent! chunk)
                  (step s))))

            ;; Returning the next elements by applying one item
            ;; from the seq at a time to the transducing function.
            (next-elem [s]
              (loop [s s buf (clear buf)]
                (cond
                  ;; Halting transduction.
                  (reduced? buf)
                  (seq (persistent! (xf (deref chunk))))

                  ;; The buffer has at least one item, return.
                  (clojure.lang.Numbers/isPos (buffer-size buf))
                  (return-step s buf)

                  :else
                  (recur (next s) (xf buf (first s))))))

            (next-chunk [s]
              (let [cf (chunk-first s)
                    chunk (unreduced (.reduce cf rrf (clear buf)))]
                (if (reduced? chunk)
                  (seq (persistent! (xf (deref chunk))))
                  (return-step (chunk-next s) chunk))))

            ;; Gets the next chunk of elements from either
            ;; a chunked seq of a non-chunked seq.
            (step [s]
              (if (nil? s)
                (seq (persistent! (xf (clear buf))))
                (if (chunked-seq? s)
                  (next-chunk s)
                  (next-elem  s))))]
      (lazy-seq
        (when-some [s (seq coll)]
          (step s))))))
