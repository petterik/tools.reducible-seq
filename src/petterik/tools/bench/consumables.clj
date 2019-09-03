(ns petterik.tools.bench.consumables
  (:require
    [petterik.tools.bench.seqs :as bench.seqs :refer [*size*]]
    [clojure.string :as string]))


(def max-power 64)
(def form-counts (->> (range)
                   (map (comp long #(Math/pow 2 %)))
                   (take-while #(<= % max-power))))

(def sizes (if bench.seqs/quick-round?
             [1000]
             [10 1000 10000]))

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
          data (one-if-quick [(range size) (repeat size 0)])
          ctor (one-if-quick [vec identity #_set])
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
         :seq?        true}
        `[(consumable!)
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
               (string/replace #"[^A-Za-z0-9]" "")
               (->> (str "_bench_"))
               (symbol))]
    `(def ~(with-meta sym# {::bench true})
       (fn []
         (bench-datasets ~@forms)))))

(defn all []
  (bench.seqs/all))

(defn quarter
  ^long []
  (long (/ ^long *size* 4)))

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
    (fn [x]
      (when (zero? (vswap! ring
                     (fn [^long x]
                       (rem ^long (inc x) n))))
        x))))

(defn nth-part [n]
  (fn [x]
    (long (/ x n))))

(defn idx-identity [idx x]
  x)

(defn idx-inc [idx x]
  (inc x))


(defbench (map identity))

(defbench (map inc))

(defbench (filter any?))
(defbench (filter (every-nth 8)))

(defbench (take (all)))
(defbench (take (quarter)))

(defbench (take-while any?))
(defbench (take-while (quarter-pred)))

(defbench (drop (all-evenly)))
(defbench (drop (quarter-evenly)))
(defbench (drop 1))

(defbench (drop-while any?))
(defbench (drop-while (quarter-evenly-pred)))
(defbench (drop-while (counting-pred 1)))

(defbench (take-nth (all)))
(defbench (take-nth (quarter)))
(defbench (take-nth 1))

(defbench (distinct))

(defbench (interpose nil))

(defbench (partition-by identity))
(defbench (partition-by (nth-part 48)))

(defbench (partition-all 1))
(defbench (partition-all 32))

(defbench (map-indexed idx-identity))
(defbench (map-indexed idx-inc))

(defbench (keep identity))
(defbench (keep (quarter-evenly-pred)))
(defbench (keep (every-nth 48)))

(defbench (keep-indexed idx-identity))
(defbench (keep-indexed (quarter-evenly-pred)))

(defbench (dedupe))

(defbench (mapcat (fn [x] (range (min 100 x)))))

(defn -main [& args]
  ;; *ns* is not set when main is called. Who knew?
  (in-ns 'petterik.tools.bench.consumables)
  (doseq [[k v] (ns-publics *ns*)
          :when (::bench (meta v))]
    (v)))

(comment

  (macroexpand-1 '(defbench (map identity)))
  (defbench (map identity))

  (doseq [[_ dataset] datasets]
    (bench-consumables dataset (map identity)))

  (petterik.tools.bench.consumables/bench-forms
    "((clojure.core/map clojure.core/identity))"
    {:type "LongRange", :size 10}
    (clojure.core/get-in petterik.tools.bench.consumables/datasets ["LongRange:10" :data])
    [(clojure.core/identity)]
    ((clojure.core/map clojure.core/identity)))

  (bench-forms :map/identity
    {:consumable? true}
    data
    (consumable!)
    (map identity))
  (bench-forms :map/identity
    {:consumable? true
     }
    data
    (consumable!)
    (map identity))

  (macroexpand '(bench-forms :map/identity
                  {:consumable? true}
                  v
                  (consumable!)
                  (map identity)))
  )
