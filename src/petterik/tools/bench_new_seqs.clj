(ns petterik.tools.bench-new-seqs
  (:require
    [petterik.tools.bench.common :as common]))

(defn -main [& args]
  (let [arg (first args)]
    (condp = arg
      "stacked-seq"
      (common/run
        (fn [n]
          (->> (range n)
            (common/maybe-dechunk)
            (map inc)
            (map inc)
            (map inc)
            (map inc)
            (map inc)
            (clojure.lang.RT/asStackedSeq)
            (reduce + 0))))

      "consumable"
      (common/run
        (fn [n]
          (->> (range n)
            (common/maybe-dechunk)
            (map inc)
            (map inc)
            (map inc)
            (map inc)
            (map inc)
            (clojure.lang.RT/asConsumable)
            (reduce + 0))))

      "xf-seq"
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
    ))
