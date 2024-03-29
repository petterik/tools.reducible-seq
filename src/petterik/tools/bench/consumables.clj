(ns petterik.tools.bench.consumables
  (:require
    [petterik.tools.bench.seqs :as bench.seqs]
    [clojure.string :as string]))


(def max-power 16)
(def form-counts
  (if bench.seqs/quick-round?
    [8]
    [16]))

(def sizes
  (if bench.seqs/quick-round?
    [1000]
    ;; TODO: two data points is not enough... lol.
    ;; TODO: Figure out which things to re-run.
    #_[100 1000 10000 1000000]
    [100000]))

(def ^:dynamic *form-count*)

(defmacro bench-forms [id descriptor coll pre-reduce forms]
  {:pre [(vector? pre-reduce)
         (integer? (:size descriptor))]}
  (let [opts# (merge {:main "consumables"} descriptor)]
    `(do
       ~@(for [form-count# form-counts]
           `(binding [*form-count* ~form-count#]
              (bench.seqs/run-bench*
               ~id
               ~[(:size opts#)]
               ~(assoc opts# :form-count form-count#)
               (->> ~coll
                 ~@(mapcat identity (repeat form-count# forms))
                 ~@pre-reduce)))))))

(defn one-if-quick [coll]
  (cond->> coll
    bench.seqs/quick-round?
    (take 1)))

(def datasets
  (into {}
    (for [size sizes
          data (one-if-quick [(range size) (repeat size 0) (vec (repeat size 0))])
          ctor (one-if-quick [#_vec doall #_set])
          :let [dataset (ctor data)
                type (.getSimpleName (type dataset))
                id (str type ":" size)]]
      [id {:size   size
           :type   type
           :data   dataset
           :getter `(get-in datasets [~id :data])}])))

(def has-consumable?
  (try
    (some?
      (requiring-resolve 'clojure.core/consumable!))
    (catch Throwable _
      false)))

(def benches
  (into [[{} `[(identity)]]]
    (when has-consumable?
      [[{:consumable? true} `[(consumable!)]]
       [{:consumable? true
         :seq?        true} `[(consumable!)
                                      (seq)]]])))

(defmacro bench-consumables [dataset forms]
  `(do
     ~@(for [[opts pre-reduce] benches]
         (let [id# (str forms)
               {:keys [type size getter]} dataset]
           `(bench-forms ~id#
              ~(assoc opts :type type :size size)
              ~getter
              ~pre-reduce
              ~forms)))))

(defmacro bench-datasets [& forms]
  `(do
     (println
       "Running bench: " ~(str forms)
       "..."
       (when bench.seqs/quick-round? "(quickly!)"))
     ~@(for [[_ dataset] datasets]
         `(bench-consumables ~(dissoc dataset :data) ~forms))))

(defmacro defbench [& forms]
  (let [sym# (-> (str forms)
               (string/replace #"[^A-Za-z0-9]" "_")
               (string/replace #"_+" "_")
               (->> (str "_bench_"))
               (symbol))]
    `(def ~(with-meta sym# {::bench true})
       (fn []
         (bench-datasets ~@forms)))))

(defn size []
  (bench.seqs/all))

(defn quarter
  ^long []
  (long (/ ^long bench.seqs/*size* 4)))

(defn- counting-pred
  "Returns x or nil such that it can be used with keep"
  [^long n]
  (let [counter (volatile! 0)]
    (fn self
      ([x]
       (when (< n ^long (vswap! counter inc))
         x))
      ([idx x]
       (self x)))))

(defn quarter-pred []
  (counting-pred (quarter)))

(defn all-evenly []
  (max 1
    (long
      (/ (bench.seqs/all)
        *form-count*))))

(defn quarter-evenly []
  (max 1 (long (/ (all-evenly) 4))))

(defn quarter-evenly-pred []
  (counting-pred (quarter-evenly)))

(defn every-nth [^long n]
  (let [ring (volatile! -1)]
    (fn self
      ([x]
       (when (zero? (vswap! ring
                      (fn [^long x]
                        (rem ^long (inc x) n))))
         x))
      ([idx x]
       (self x)))))

(defn idx-identity [idx x]
  x)

(defn idx-inc [idx x]
  (inc x))

(defn range-min-max [low high]
  (fn [x] (range (min high (max low x)))))

(defn log-of-size
  ^long []
  (long (Math/log10 (bench.seqs/all))))

(defbench (map identity))

#_(defbench (map inc))

(defbench (filter any?))
(defbench (filter (every-nth (log-of-size))))

(defbench (take (size)))
#_(defbench (take (quarter)))

(defbench (take-while any?))
#_(defbench (take-while (quarter-pred)))


(defbench (drop (all-evenly)))
#_(defbench (drop (quarter-evenly)))
(defbench (drop 1))


(defbench (drop-while any?))
#_(defbench (drop-while (quarter-evenly-pred)))
(defbench (drop-while (counting-pred 1)))

#_(defbench (take-nth (size)))
#_(defbench (take-nth (quarter)))
(defbench (take-nth 1))
(defbench (take-nth (log-of-size)))


(defbench (distinct))

(defbench (interpose nil) (take (size)))

(defbench (partition-by identity))

(defbench (partition-all 1))
(defbench (partition-all (log-of-size)))

(defbench (map-indexed idx-identity))
#_(defbench (map-indexed idx-inc))

(defbench (keep identity))
#_(defbench (keep (quarter-evenly-pred)))
(defbench (keep (every-nth (log-of-size))))

#_(defbench (keep-indexed idx-identity))
(defbench (keep-indexed (every-nth (log-of-size))))


(defbench (dedupe))

(defbench (mapcat (range-min-max (log-of-size) 50)) (take (size)))

(defn -main [& args]
  ;; *ns* is not set when main is called. Who knew?
  (in-ns 'petterik.tools.bench.consumables)
  (doseq [[_ v] (into (sorted-map)
                  (filter (comp ::bench meta val))
                  (ns-publics *ns*))]
    (v)))
