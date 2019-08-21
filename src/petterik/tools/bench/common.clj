(ns petterik.tools.bench.common)

(defn dechunk [s]
  (lazy-seq
    (when-some [[x & s] (seq s)]
      (cons x (dechunk s)))))

(def dechunk?
  (memoize
    (fn []
      (Boolean/parseBoolean
        (System/getProperty "petterik.bench.dechunk")))))

(defn maybe-dechunk [coll]
  (if (dechunk?)
    (dechunk coll)
    coll))

(defn run
  ([f]
   (run f 10000 100 100))
  ([f size inner outer]
   #_(crit/warmup-for-jit crit/s-to-ns #(f size))
   (dotimes [i outer]
     (dotimes [_ inner]
       (f size))
     (prn "Run: " (* i inner)))))
