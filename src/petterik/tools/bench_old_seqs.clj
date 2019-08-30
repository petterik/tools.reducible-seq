(ns petterik.tools.bench-old-seqs
  (:require
    [petterik.tools.bench.common :as common]))

(defn -main [& args]
  (common/main args identity))