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

;;;;;;;;;
;; Runs
;;

(defn run:identity [pre-reduce]
  (run
    (fn [n]
      (->> (repeat n nil)
        (maybe-dechunk)
        (map identity)
        (map identity)
        (map identity)
        (map identity)
        pre-reduce
        (reduce (fn [x _] x) nil)))))

(defn long-inc ^long [^long n]
  (clojure.lang.Numbers/inc n))

(defn run:type-hints [pre-reduce]
  (run
    (fn [n]
      (->> (range n)
        (maybe-dechunk)
        (map long-inc)
        (map long-inc)
        (map long-inc)
        (map long-inc)
        pre-reduce
        (reduce (fn ^long [^long a ^long b]
                  (clojure.lang.Numbers/add a b))
          0)))))

(defn main [args pre-reduce-fn]
  (let [run-id (or (first args) "identity")
        run (condp = run-id
              "identity" run:identity
              "type-hints" run:type-hints)]
    (prn "Running " run-id " with " pre-reduce-fn)
    (run pre-reduce-fn)))