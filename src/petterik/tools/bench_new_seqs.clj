(ns petterik.tools.bench-new-seqs
  (:require
    [petterik.tools.bench.common :as common]))

(defn -main [& args]
  (common/main (rest args)
    (condp = (first args)
      "stacked-seq"
      (fn as-stacked-seq [x]
        (clojure.lang.RT/asStackedSeq x))

      "consumable"
      (fn as-consumable [x]
        (clojure.lang.RT/asConsumable x))

      "xf-seq"
      identity)))
