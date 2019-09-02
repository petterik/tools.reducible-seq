(ns petterik.tools.bench.seqs
  "Namespace for benchmarking seqs."
  (:require
    [criterium.core :as crit]
    [petterik.tools.bench.common :as common]))

(comment
  (crit/quick-bench (inc 1))
  (crit/quick-benchmark (inc 1) {})

  (def res *1)

  (crit/scale-time )
  (first (:mean res))
  (first (:lower-q res))
  (first (:upper-q res)))

(def max-size (long 1e6))
(def ^:dynamic *size* nil)
(def clj-version (clojure-version))
(def java-version (System/getProperty "java.version"))
(def chunked? (not (common/dechunk?)))


(def rf-nil
  (fn
    ([])
    ([x] x)
    ([x _] x)))

(def sizes (take-while #(<= % max-size) (iterate #(* 10 %) 10)))

(defmacro run-bench [id expr]
  `(let [id# ~id]
     (println "Running bench: " id# " ...")
     (vec
       (for [size# sizes]
         (binding [*size* size#]
           (let [res# (crit/benchmark ~(list `reduce `rf-nil nil `(common/maybe-dechunk ~expr)) {})
                 ret# (into {:id           id#
                             :size         size#
                             :clj-version  clj-version
                             :java-version java-version
                             :chunked?     chunked?}
                        (map #(update % 1 first))
                        (select-keys res# [:mean :lower-q :upper-q]))
                 file# (format "bench-%s-%s-%s.edn"
                         (if chunked? "chunked" "dechunked")
                         clj-version
                         java-version)]
             (spit file# ret# :append true)
             (spit file# "\n" :append true)
             ret#))))))

(defn ^:static divisible [div]
  (fn [^long num]
    (zero? (rem num div))))

(defn all []
  *size*)

(defn half []
  (long (/ *size* 2)))

(defn quarter []
  (long (/ *size* 4)))

(defn all-pred []
  any?)

(defn half-pred []
  (fn [^long num]
    (< num (long (/ *size* 2)))))

(defn quarter-pred []
  (fn [^long num]
    (< num (long (/ ^long *size* 4)))))


(def nil-vectors (into {}
                   (map (juxt identity #(into [] (repeat % nil))))
                   sizes))

(defn nils []
  (or
    (get nil-vectors *size*)
    (throw (ex-info
             (str "Unable to get a vector of nils for size: " *size*)
             {:size            *size*
              :nil-vector-keys (keys nil-vectors)}))))

(def ranges (into {}
              (map (juxt identity #(doall (range %))))
              sizes))

(defn nums []
  (get ranges *size*))

;; TODO: Rebind repeat and range to maybe dechunk
(defn -main [& args]
  (run-bench :map/identity (map identity (nils)))
  (run-bench :map/inc (map inc (nums)))

  (run-bench :filter/all (filter (divisible 1) (nums)))
  #_(run-bench :filter/half (filter (divisible 2) (nums)))
  (run-bench :filter/quarter (filter (divisible 4) (nums)))

  (run-bench :take/all (take (all) (nils)))
  #_(run-bench :take/half (take (half) (nils)))
  (run-bench :take/quarter (take (quarter) (nils)))

  (run-bench :take-while/all (take-while (all-pred) (nums)))
  #_(run-bench :take-while/half (take-while (half-pred) (nums)))
  (run-bench :take-while/quarter (take-while (quarter-pred) (nums)))

  (run-bench :drop/all (drop (all) (nils)))
  #_(run-bench :drop/half (drop (half) (nils)))
  (run-bench :drop/quarter (drop (quarter) (nils)))
  (run-bench :drop/one (drop 1 (nils)))

  (run-bench :drop-while/all (drop-while (all-pred) (nums)))
  #_(run-bench :drop-while/half (drop-while (half-pred) (nums)))
  (run-bench :drop-while/quarter (drop-while (quarter-pred) (nums)))
  (run-bench :drop-while/one (drop-while #{0} (nums)))

  (run-bench :take-nth/all (take-nth (all) (nils)))
  #_(run-bench :take-nth/half (take-nth (half) (nils)))
  (run-bench :take-nth/quarter (take-nth (quarter) (nils)))

  (run-bench :distinct/numbers (distinct (nums)))
  (run-bench :distinct/tenth (let [v (get nil-vectors (long (/ *size* 10)) [])]
                               (distinct (apply concat (repeat 10 v)))))

  (run-bench :interpose/nils (interpose nil (nils)))

  ;; half-pred is correct for "all", as it'll return a new value for
  ;; each new item.
  (run-bench :partition-by/all (partition-by (half-pred) (nums)))
  (run-bench :partition-by/half (partition-by (quarter-pred) (nums)))

  (run-bench :partition-all/size-1 (partition-all 1 (nils)))
  #_(run-bench :partition-all/size-8 (partition-all 8 (nils)))
  (run-bench :partition-all/size-32 (partition-all 32 (nils)))
  #_(run-bench :partition-all/size-128 (partition-all 128 (nils)))

  (run-bench :map-indexed/identity (map-indexed #(do %2) (nils)))
  (run-bench :map-indexed/inc (map-indexed #(inc %2) (nums)))

  (run-bench :keep/all (keep identity (nums)))
  #_(run-bench :keep/half (keep (half-pred) (nums)))
  (let [pred (quarter-pred)]
    (run-bench :keep/quarter (keep #(when (pred %) %) (nums))))

  (run-bench :keep-indexed/all (keep-indexed (let [pred (all-pred)] #(pred %2)) (nums)))
  #_(run-bench :keep-indexed/half (keep-indexed (let [pred (half-pred)] #(pred %2)) (nums)))
  (run-bench :keep-indexed/quarter (keep-indexed (let [pred (quarter-pred)] #(pred %2)) (nums)))

  (run-bench :dedupe/none (dedupe (nums)))
  #_(run-bench :dedupe/half (dedupe (repeat 2 (long (/ *size* 2)))))
  #_(run-bench :dedupe/quarter (dedupe (repeat 4 (long (/ *size* 4)))))

  (println (format "Done benchmarking %s %s %s!" chunked? clj-version java-version))
  )