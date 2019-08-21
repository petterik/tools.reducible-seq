(ns petterik.tools.bench-old-seqs
  (:require
    [petterik.tools.bench.common :as common]))

(defn -main [& args]
  (common/run
    (fn [n]
      (->> (range n)
        (common/maybe-dechunk)
        (map inc)
        (map inc)
        (map inc)
        (map inc)
        (map inc)
        (reduce + 0)))))