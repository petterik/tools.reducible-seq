(ns petterik.tools.vol-xf-seq)

;; xf-seq with volatile single space buffer.

(def ^:static xf-seq
  (let [jank-val! (fn [^clojure.lang.Volatile vol]
                    (let [v (.deref vol)]
                      (.reset vol ::xf-seq-nothing)
                      v))

        val-set! (fn
                   ([])
                   ([x] x)
                   ([^clojure.lang.Volatile vol x]
                    (.reset vol x)
                    x))
        ;; TODO: Continue this volatile path...
        step (fn self [s xf ^clojure.lang.Volatile vol]
               (when s
                 (if (chunked-seq? s)
                   (let [cf (chunk-first s)
                         size (.count cf)
                         cb (chunk-buffer size)
                         s (loop [i 0]
                             (if (== i size)
                               (chunk-rest s)
                               (let [ret (xf vol (.nth cf i))
                                     x (jank-val! vol)]
                                 (when-not (= ::xf-seq-nothing x)
                                   (chunk-append cb x))
                                 (if (clojure.lang.RT/isReduced ret)
                                   ()
                                   (recur (inc i))))))]
                     (if (clojure.lang.Numbers/isZero (.count cb))
                       (recur (.seq ^clojure.lang.ISeq s) xf vol)
                       (chunk-cons
                         (chunk cb)
                         (lazy-seq (self (.seq ^clojure.lang.ISeq s) xf vol)))))
                   (let [ret (xf vol (.first ^clojure.lang.ISeq s))
                         x (jank-val! vol)]
                     (if (clojure.lang.RT/isReduced ret)
                       (if (= ::xf-seq-nothing x)
                         nil
                         (cons x nil))
                       (if (= ::xf-seq-nothing x)
                         (recur (.next ^clojure.lang.ISeq s) xf vol)
                         (cons x
                           (lazy-seq
                             (self (.next ^clojure.lang.ISeq s) xf vol)))))))))]
    ;; TODO: Why is (->> (range 1e6) (map str) (mapcat seq) (map int)) so slow?
    (fn [xform coll]
      (lazy-seq
        (let [s (seq coll)]
          (when s
            (step s (xform val-set!) (clojure.lang.Volatile. ::xf-seq-nothing))))))))


(comment


  )